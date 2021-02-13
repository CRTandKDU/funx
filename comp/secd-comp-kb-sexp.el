(defun secd-comp--args-lazy (elist n c)
  "Compiles a list of expressions in order stated (in application forms)."
  ;; (insert (format "\t--- comp--args\n\te: %s\n\tn: %s\n\tc: %s\n" elist n c))
  
  (if (eq elist nil) c
    (secd-comp--args-lazy (cdr elist) n (secd-comp--comp-lazy (car elist) n c))
    )
  )


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
  (secd-comp--comp-lazy-aux e n c))

(defun secd-comp--comp-lazy-aux (e n c)  
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
		;; jmc 2021-01-20 (set VAR CST) -> (set VAR SEXP)
		(if (eq (car e) 'set)
		    (let ((expression (car (cdr (cdr e)))))
		      (add-to-list
		       n
		       (cons secd--kb-RHS-set-variable (car (cdr e)))) 
		      (secd-comp--comp-lazy
		       expression
		       n
		       ;; (cons 'LDC (cons (car (cdr e)) (cons 'SET c))))
		       (cons 'LDC (cons (car (cdr e)) (cons 'SET c)))))
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
					(secd-comp--args-lazy
					 (cdr e)
					 n
					 (secd-comp--comp-lazy (car e) n (cons 'AP c)))
					;; Done
					)
				      )
				    )
				  ))))))))))))))))
    )

(provide 'secd-comp-kb-sexp)
