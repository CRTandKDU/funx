;;; A functional compiler variant:  compiling knowledge bases to environments
(require 'secd-env-group)
(require 'secd-cps-group)
(require 'secd-exec)
(require 'secd-comp)

;; OR-AND Tree decorations: global alists, stored in compiled environment
;; Forward chaining decorations
(defconst secd--kb-forward-chaining-signs  '*FWRD-SIGNS*)
(defconst secd--kb-forward-chaining-rules  '*FWRD-RULES*)
(defconst secd--kb-backward-chaining-signs '*BWRD-SIGNS*)
;; Backward chaining decorations
(defconst secd--kb-RHS-set-variable  '*RHS*)
(defconst secd--kb-LHS-variable  '*LHS*)

;; WHAT-IF decorations

;; Control options
;; If true, rule values post their hypo for evaluation only if `*T*' (gating on)
(defvar secd--kb-option-forward-chaining-gate t)
;; If true, when signs need evaluation backward immediately on rules the RHS
;; of which `set's the signs. (Note: backward on hypos is automatic.)
(defvar secd--kb-option-backward-chaining-rhs nil)

(defun secd-compile-sexp--lazy (e c)
  "Compiles sexp `e' with continuation `c' and all variables bound to promises.
Returns a cons of control list and LHS, RHS variable names found in sexp `e' in an alist with keys `*LHS' and `*RHS*' respectively."
  (let* ((lhs-names nil) (rhs-names nil) (names nil)
	 (clist (secd-comp--comp-lazy e 'names c))
	 )
    ;; Splits between LHS and RHS
    (dolist (deco names names)
      (if (eq secd--kb-LHS-variable (car deco))
	  (add-to-list 'lhs-names (cdr deco)))
      (if (eq secd--kb-RHS-set-variable (car deco))
	  (add-to-list 'rhs-names (cdr deco)))
      )
    (cons  clist (cons lhs-names rhs-names))
    )
  )

(defun secd-comp--comp-lazy (e n c)
  "Compiles expression `e', with names (quoted) `n' and continuation c.
Adds variables to alist of names in `n', altering it.
"
  ;; (insert (format "--\ne: %s\nn: %s\nc: %s\n" e n c))
  ;; 0 arg
  (if (atom e) (add-to-list n (cons secd--kb-LHS-variable e)))
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
	    (if (eq (car e) 'not)
		(secd-comp--comp-lazy (car (cdr e)) n (cons 'NOT c))
	      ;; 2 args
	      (if (eq (car e) 'cons)
		  (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'CONS c)))
		;; Special processing for `set'.
		(if (eq (car e) 'set)
		    (secd-comp--comp-lazy (car (cdr (cdr e))) (add-to-list n (cons secd--kb-RHS-set-variable (car (cdr e)))) (cons 'LDC (cons (car (cdr e)) (cons 'SET c))))
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
				  ))))))))))))))))
    )

;; KB compilers
(defun secd-comp--rule2env (kb rn)
  "Compiles an environment and a list of terminals for rule `rn' in `kb'.
Returns environment and list of terminals found in conditions."
  (let ((env nil))
    (if (eq (car kb) 'rule)
	(let ((rclist nil) (cclist nil) (aclist nil) (axlist nil)
	      (rhs-rvars nil) (rvars  nil))
	  ;; Compile LHS conditions
	  ;; Push each condition in environment. Accumulate rule, variables.
	  (dolist (c (car (cdr (cdr kb))))
	    (let ((cn (gensym 'C))
		  (ccompiled (secd-compile-sexp--lazy c '(UPD)))
		  )
	      (setq rclist (cons 'LDP (cons cn rclist)))
	      (setq cclist (push (cons cn (car ccompiled)) cclist))
	      ;; (setq rvars  (append rvars (cadr ccompiled)))
	      ;; Prepare list of signs -> condition forward decorations
	      ;; with the exception of constant promises `*T*' and `*F*'
	      (dolist (var (cadr ccompiled) rvars)
		(if (and (not (equal var secd--ops-true))
			 (not (equal var secd--ops-false)))
		    (push (cons var (cons cn rn)) rvars)))
	      ;; (insert (format "---\nc: %s\nrvars :%s\ncclist: %s\n" c rvars cclist))
	      )
	    )
	  ;; Compile RHS actions if any
	  (dolist (ax (car (cdr (cdr (cdr kb)))))
	    (let ((axn (gensym 'A))
		  (axcompiled (secd-compile-sexp--lazy ax '(UPD)))
		  )
	      (setq axlist (cons 'LDP (cons axn axlist)))
	      (setq aclist (push (cons axn (car axcompiled)) aclist))
	      ;; (setq rvars  (append rvars (cadr axcompiled)))
	      (dolist (var (cadr axcompiled) rvars)
		(push (cons var (cons axn rn)) rvars))
	      (setq rhs-rvars  (append rhs-rvars (cddr axcompiled))) 
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
	  (cons env (cons rvars rhs-rvars))
	  )
      nil
      )
    )
  )

(defun secd-comp--kb2env (kb)
  "Compiles a complete environment for knowledge base `kb'."
  ;; Adds typed graph edges in several passes
  (let (
	(env nil)     ;; Environment incrementally built from rule sexps
	(hypos nil)   ;; Collect hypotheses
	(signs nil)   ;; Collect signs, i.e. non-hypo terminals in conds
	(flist nil)   ;; Forward-chaining signs -> rule alist
	(frlst nil)   ;; Forward-chaining rule -> hypo alist
	(blist nil)   ;; Backward-chaining set-variable -> rule
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
	;; Separate lists for bwrd on set-variables and fwrd on signs
	;; Merge (var Ci Rj) from different rules
	(dolist (var-c-r (cadr rcompiled) flist)
	  ;; (if (assoc var flist) (push rn (cdr (assoc var flist)))
	  ;;   (push (cons var (cons rn nil)) flist)))
	  (if (assoc (car var-c-r) flist)
	      (push (cdr var-c-r) (cdr (assoc (car var-c-r) flist)))
	    (push (cons (car var-c-r) (list (cdr var-c-r))) flist)))
	(dolist (var (cddr rcompiled) blist)
	  (if (assoc var blist) (push rn (cdr (assoc var blist)))
	    (push (cons var (cons rn nil)) blist)))
	(dolist (var (cadr rcompiled) signs) (add-to-list 'signs (car var)))
	(dolist (var (cddr rcompiled) signs) (add-to-list 'signs var))
	)
      )
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 1:\nH: %s\nE: %s\nT: %s\nF: %s\nB: %s\n" hypos env signs flist blist))
      )
    ;; Pass #2
    ;; Increment environment with terminals (both from LHS and from RHS)
    ;; Create *BWRD-SIGNS*
    (push (cons secd--kb-backward-chaining-signs blist) env )
    ;; Push constant promises
    (push (cons secd--ops-true
		(cons 'LDC (cons secd--ops-true (cons 'UPD nil))))
	  env)
    (push (cons secd--ops-false
		(cons 'LDC (cons secd--ops-false (cons 'UPD nil))))
	  env)

    (dolist (sign signs env)
      ;; Do nothing if sign is an hypo, backward chaining is built-in
      (if (null (assoc sign hypos))
	  ;; Do we backward chain on RHS settable signs?
	  (let ((rhs-rules
		 (cdr (assoc sign (cdr (assoc secd--kb-backward-chaining-signs env)))))
		)
	    (if (and secd--kb-option-backward-chaining-rhs rhs-rules)
		(let* ((n (length rhs-rules))
		       ;; (ctrls (list 'ANY n 'SEL '(JOIN) (list 'ASK sign 'JOIN) 'UPD))
		       (ctrls (list 'ANY n
				    'SEL
				    (list 'LDP sign 'JOIN)
				    (list 'ASK sign 'JOIN) 'UPD))
		      )   
		  (save-current-buffer
		    (set-buffer (get-buffer-create "*SECD-COMP*"))
		    (insert (format "---RHS B-C:\nS: %s (%d rules)\n" sign n))
		    )
		  (dolist (r rhs-rules)
		    (push r ctrls)
		    (push 'LDP ctrls)
		    )
		  (push (cons sign ctrls) env)
		  )
	      (push (cons sign (cons 'ASK (cons sign (cons 'UPD nil)))) env))
	    )
	;; No: standard ASK <SIGN> UPD control list
	(push (cons sign (cons 'ASK (cons sign (cons 'UPD nil)))) env)
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
  (save-current-buffer
    (set-buffer (get-buffer-create "*SECD*"))
    (let ((cstr (format "#%02X%02X%02X" 0 255 128)))
      (insert
       (propertize
	(format "FWRD: On %s (%s): %s\n" var val (car (last (secd--d state))))
	'face `(foreground-color . ,cstr))
       )
      )
    )
  ;; Post delayed hypos from rules
  (if (assoc var (cdr (assoc secd--kb-forward-chaining-rules (secd--e state))))
      (if (or (null secd--kb-option-forward-chaining-gate)
	      (and  secd--kb-option-forward-chaining-gate
		    (equal val secd--ops-true)))
	  (let ((d (car (last (secd--d state))))
		(hypos (cdr (assoc var (cdr (assoc secd--kb-forward-chaining-rules (secd--e state)))))))
	    (save-current-buffer
	      (set-buffer (get-buffer-create "*SECD*"))
	      (insert (format "FWRD GATE:%s (%s): %s\n" var val (car (last (secd--d state))))))
	    
	    (dolist (hypo hypos d)
	      (when (listp (cdr (assoc hypo (secd--e state))))
		(secd--cps-set-bot 'LDP d)
		(secd--cps-set-bot hypo d)
		(secd--cps-set-bot 'AP0 d))
	      )
	    )
	)
    )	  
  ;; Post delayed condition then rules, from signs
  (let ((d (car (last (secd--d state))))
	(cond-rule-list
	 (cdr (assoc var (cdr (assoc secd--kb-forward-chaining-signs (secd--e state)))))))
    (dolist (c-r cond-rule-list d)
      (when (listp (cdr (assoc (car c-r) (secd--e state))))
	(secd--cps-set-bot 'LDP d)
	(secd--cps-set-bot (car c-r) d)
	(secd--cps-set-bot 'AP0 d)
	(secd--cps-set-bot 'LDP d)
	(secd--cps-set-bot (cdr c-r) d)
	(secd--cps-set-bot 'AP0 d)
	)
      )
    )
  (save-current-buffer
    (set-buffer (get-buffer-create "*SECD*"))
    (insert (format "FWRD:    %s (%s): %s\n" var val (car (last (secd--d state))))))
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
