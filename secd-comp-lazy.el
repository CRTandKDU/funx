;;; Lazy compilers and variants
(require 'secd-comp)

(defun secd-compile-sexp--lazy (e c)
  "Compiles sexp `e' with continuaton `c' with variable bound to promises.
Returns control list and variable names in sexp."
  (let* ((names nil)
	 (clist (secd-comp--comp-lazy e 'names c))
	 )
    (cons  clist names))
  )

(defun secd-comp--comp-lazy (e n c)
  "Compiles lazily expression `e', with names (quoted) `n' and continuation c.
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
	      (if (eq (car e) 'eq)
		  (secd-comp--comp-lazy (car (cdr (cdr e))) n (secd-comp--comp-lazy (car (cdr e)) n (cons 'EQ c)))
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
    )))))))))))))
  )

;; KB compilers
(defun secd-comp--rule2env (kb rn)
  "Compiles an environment and a list of terminals for rule `rn' in `kb'.
Returns environment and list of terminals found in conditions."
  (let ((env nil))
    (if (eq (car kb) 'rule)
	(let ((rclist nil) (cclist nil) (rvars  nil))
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
	  (setq rclist (append rclist (cons 'ALL (cons (/ (length rclist) 2) (cons 'UPD nil)))))
	  (push (cons rn rclist) env)
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
	(env nil)
	(hypos nil)
	(signs nil)
	)
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (erase-buffer)
      (insert (format "-- NEW COMPILE:\n%s\n" kb))
      )
    ;; Pass #1
    ;; Accumulate hypos and associated rules in `hypos'.
    ;; Compiles rules into `env', collecting terminals in `signs'.
    (dolist (rule kb env)
      (let* (
	     (rn (gensym 'R))
	     (rcompiled (secd-comp--rule2env rule rn))
	    )
	(if (assoc (cadr rule) hypos) (push rn (cdr (assoc (cadr rule) hypos)))
	  (push (cons (cadr rule) (cons rn nil)) hypos))
	(setq env (append env (car rcompiled)))
	(dolist (var (cdr rcompiled) signs) (add-to-list 'signs var))
	)
      )
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 1:\nH: %s\nE: %s\nT: %s\n" hypos env signs))
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
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 3:\nH: %s\nE: %s\nT: %s\n" hypos env signs))
      )
    env
    )
  )

(provide 'secd-comp-lazy)
