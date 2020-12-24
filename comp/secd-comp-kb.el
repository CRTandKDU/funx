;;; A compiler variant for compiling knowledge bases to environments
(require 'secd-env-group)
(require 'secd-cps-group)
(require 'secd-exec)
(require 'secd-comp)

;; OR-AND Tree decorations: global alists, stored in compiled environment
;; Forward chaining decorations
(defconst secd--kb-forward-chaining-signs  '*FWRD-SIGNS*)
(defconst secd--kb-forward-chaining-rules  '*FWRD-RULES*)
;; WHAT-IF decorations

;; Control options
;; If true, rule values post their hypo evaluation only if `*T*' (gating on)
(defvar secd--kb-option-forward-chaining-gate t)

(defun secd-compile-sexp--lazy (e c)
  "Compiles sexp `e' with continuation `c' and all variables bound to promises.
Returns a cons of control list and variable names found in sexp `e'."
  (let* ((names nil)
	 (clist (secd-comp--comp-lazy e 'names c))
	 )
    (cons  clist names))
  )

(defun secd-comp--comp-lazy (e n c)
  "Compiles expression `e', with names (quoted) `n' and continuation c.
Adds variables to list of names in `n', altering it.
"
  ;; (insert (format "--\ne: %s\nn: %s\nc: %s\n" e n c))
  ;; 0 arg
  (if (atom e) (add-to-list n e))
  (if (atom e) (cons 'LDP (cons e (cons 'AP0 c)))
    ;; 1 arg
    (if (eq (car e) 'car)
	(secd-comp--comp-lazy (car (cdr e)) n (cons 'CAR c))
      (if (eq (car e) 'cdr)
	  (secd-comp--comp-lazy (car (cdr e)) n (cons 'CDR c))
	(if (eq (car e) 'atom)
	    (secd-comp--comp-lazy (car (cdr e)) n (cons 'ATOM c))
	  (if (eq (car e) 'quote)
	      (cons 'LDC (cons (car (cdr e)) c))
	    ;; 2 args
	    (if (eq (car e) 'cons)
		(secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'CONS c)))
	    (if (eq (car e) 'set)
		(secd-comp--comp-lazy (car (cdr (cdr e))) (add-to-list n (car (cdr e))) (cons 'LDC (cons (car (cdr e)) (cons 'SET c))))
	      (if (eq (car e) 'eq)
		  (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'EQ c)))
		(if (eq (car e) 'in)
		    (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'IN c)))
		  (if (eq (car e) 'leq)
		      (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'LEQ c)))
		    (if (eq (car e) 'add)
			(secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'ADD c)))
		      (if (eq (car e) 'sub)
			  (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'SUB c)))
			(if (eq (car e) 'mul)
			    (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'MUL c)))
			  (if (eq (car e) 'div)
			      (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'DIV c)))
			    (if (eq (car e) 'rem)
				(secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'REM c)))
			      ;; 3 args
			      (if (eq (car e) 'if)
				  ((lambda (cont-t cont-f)
				     (secd-comp--comp-lazy (car (cdr e)) n
							   (cons 'SEL (cons cont-t (cons cont-f c)))))
				   (secd-comp--comp-lazy (car (cdr (cdr e))) n '(JOIN))
				   (secd-comp--comp-lazy (car (cdr (cdr (cdr e)))) n '(JOIN)))
				;; many args
				(if (eq (car e) 'lambda)
				    (cons 'LDF
					  (cons
					   (cons (car (cdr e))
						 (secd-comp--comp-lazy (car (cdr (cdr e))) n '(RTN)))
					   c)
					  )
				  (if (eq (car e) 'let)
				      (cons 'DUM
					    (secd-comp--list
					     (car (cdr e))
					     n
					     (cons 'LDF (cons (cons (secd-comp--vars (car (cdr e))) (secd-comp--comp-lazy (car (cdr (cdr e))) n '(RTN))) (cons 'RAP c)))))
				    ;; Rest has to be an application
				    (secd-comp--args
				     (cdr e)
				     n
				     (secd-comp--comp-lazy (car e) n (cons 'AP c)))
				    ;; Done
				    )
				  )
				)
			      )))))))))))))))
    )

;; KB compilers
(defun secd-comp--rule2env (kb rn)
  "Compiles an environment and a list of terminals for rule `rn' in `kb'.
Returns environment and list of terminals found in conditions."
  (let ((env nil))
    (if (eq (car kb) 'rule)
	(let ((rclist nil) (cclist nil) (aclist nil) (axlist nil) (rvars  nil))
	  ;; Push each condition in environment. Accumulate rule, variables.
	  (dolist (c (car (cdr (cdr kb))))
	    (let ((cn (gensym 'C))
		  (ccompiled (secd-compile-sexp--lazy c '(UPD)))
		  )
	      (setq rclist (cons 'LDP (cons cn rclist)))
	      (setq cclist (push (cons cn (car ccompiled)) cclist))
	      (setq rvars  (append rvars (cdr ccompiled)))
	      ;; (insert (format "---\nc: %s\nrclist :%s\ncclist: %s\n" c rclist cclist))
	      )
	    )
	  ;; Compile RHS actions if any
	  (dolist (ax (car (cdr (cdr (cdr kb)))))
	    (let ((axn (gensym 'A))
		  (axcompiled (secd-compile-sexp--lazy ax '(UPD)))
		  )
	      (setq axlist (cons 'LDP (cons axn axlist)))
	      (setq aclist (push (cons axn (car axcompiled)) aclist))
	      (setq rvars  (append rvars (cdr axcompiled)))
	      )
	    )
	  ;; (insert (format "---\nax: %s\nrv: %s\n" axlist rvars))
	  ;; Generates: (LDP C_i ALL <n> SEL (LDP AX_i SEQ <m> LDC *T* JOIN) (LDC *F* JOIN)
	  (setq
	   rclist
	   (append
	    rclist
	    (cons
	     'ALL
	     (cons
	      (/ (length rclist) 2)
	      (if axlist
		  (cons
		   'SEL
		   (append
		    (list (append
			   axlist
			   (cons 'SEQ
				 (cons (/ (length axlist) 2)
				       (cons 'LDC (cons '*T* (cons 'JOIN nil))))))
		    (cons 'LDC (cons '*F* (cons 'JOIN nil))))
		    (cons 'UPD nil)))
		(cons 'UPD nil))))))

	  (push (cons rn rclist) env)
	  (setq env (append env aclist))
	  (setq env (append env cclist))
	  (cons env rvars)
	  )
      nil
      )
    )
  )

(defun secd-comp--kb2env (kb)
  "Compiles a complete environment for knowledge base `kb'."
  (let (
	(env nil)     ;; Environment incrementally built from rule sexps
	(hypos nil)   ;; Collect hypotheses
	(signs nil)   ;; Collect signs, i.e. non-hypo terminals in conds
	(flist nil)   ;; Forward-chaining signs -> rule alist
	(frlst nil)   ;; Forward-chaining rule -> hypo alist
	)
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (erase-buffer)
      (insert (format "-- NEW COMPILE:\n%s\n" kb))
      )
    ;; Pass #1
    ;; Accumulate hypos and associated rules in `hypos'.
    ;; Compiles rules into `env', collecting terminals in `signs'.
    ;; Forward-chaining liks are collected as an alist `(var . (rules))'
    (dolist (rule kb env)
      (let* (
	     (rn (gensym 'R))
	     (rcompiled (secd-comp--rule2env rule rn))
	     )
	(if (assoc rn frlst) (push (cadr rule) (cdr (assoc rn frlst)))
	  (push (cons rn (cons (cadr rule) nil)) frlst))
	(if (assoc (cadr rule) hypos) (push rn (cdr (assoc (cadr rule) hypos)))
	  (push (cons (cadr rule) (cons rn nil)) hypos))
	(setq env (append env (car rcompiled)))
	(dolist (var (cdr rcompiled) flist)
	  (if (assoc var flist) (push rn (cdr (assoc var flist)))
	    (push (cons var (cons rn nil)) flist)))
	(dolist (var (cdr rcompiled) signs) (add-to-list 'signs var))
	)
      )
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 1:\nH: %s\nE: %s\nT: %s\nF: %s\n" hypos env signs flist))
      )
    ;; Pass #2
    ;; Increment environment with terminals
    (dolist (sign signs env)
      (if (null (assoc sign hypos))
	  (push
	   (cons sign (cons 'ASK (cons sign (cons 'UPD nil))))
	   env
	   )
	)
      )
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 2:\nH: %s\nE: %s\nT: %s\n" hypos env signs))
      )
    ;; Pass #3
    ;; Increment environment with hypos
    (dolist (h hypos env)
      (let ((rlist nil)
	    (suffix (cons 'ANY (cons (length (cdr h)) (cons 'UPD nil))))
	    )
	(dolist (rule (cdr h) rlist)
	  (setq rlist (cons 'LDP (cons rule rlist)))
	  )
	(push (cons (car h) (append rlist suffix)) env)
	)
      )
    ;; Pass #4
    ;; Increment environment with forward-chaining links
    (push (cons secd--kb-forward-chaining-signs flist) env )
    (push (cons secd--kb-forward-chaining-rules frlst) env )
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 3:\nH: %s\nE: %s\nT: %s\n" hypos env signs))
      )
    env
    )
  )

;;; Commands: Operating the kb compiler

;; Forward-chaining hook: signs to rules, conditionally rules to hypos
(defun secd-comp--kb-forward-hook (var val state)
  (insert (format "On %s (%s): %s\n" var val (car (last (secd--d state)))))
  ;; Post hypos from rules
  (if (assoc var (cdr (assoc secd--kb-forward-chaining-rules (secd--e state))))
      (if (or (null secd--kb-option-forward-chaining-gate)
	      (and  secd--kb-option-forward-chaining-gate
		    (equal val secd--ops-true)))
	  (let ((d (car (last (secd--d state))))
		(hypos (cdr (assoc var (cdr (assoc secd--kb-forward-chaining-rules (secd--e state)))))))
	    (dolist (hypo hypos d)
	      (when (listp (cdr (assoc hypo (secd--e state))))
		(secd--cps-set-bot 'LDP d)
		(secd--cps-set-bot hypo d)
		(secd--cps-set-bot 'AP0 d))
	      )
	    )
	)
    )	  
  ;; Post rules from signs
  (let ((d (car (last (secd--d state))))
	(rules (cdr (assoc var (cdr (assoc secd--kb-forward-chaining-signs (secd--e state)))))))
    (dolist (rule rules d)
      (when (listp (cdr (assoc rule (secd--e state))))
	(secd--cps-set-bot 'LDP d)
	(secd--cps-set-bot rule d)
	(secd--cps-set-bot 'AP0 d)
	)
      )
    )
  )

(defun secd-comp--kb-knowcess (e goals &optional s)
  (let ((clist (cons 'STOP nil)))
    (dolist (goal goals clist)
      (setq clist (cons 'LDP (cons goal (cons 'AP0 clist))))
      )
    (add-hook 'secd-env-update-hook 'secd-comp--kb-forward-hook)
    (secd-cycle s e clist nil)
    )
  )


(provide 'secd-comp-kb)
