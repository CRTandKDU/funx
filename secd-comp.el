;;; Compiler

(defun comp (e n c)
  "Compiles expression e, with names n and continuation c."
  (insert (format "--\ne: %s\nn: %s\nc: %s\n" e n c))
  ;; 0 arg
  (if (atom e) (cons 'LD (cons e c))
    ;; 1 arg
    (if (eq (car e) 'car)
	(comp (car (cdr e)) n (cons 'CAR c))
      (if (eq (car e) 'cdr)
	  (comp (car (cdr e)) n (cons 'CDR c))
	(if (eq (car e) 'atom)
	    (comp (car (cdr e)) n (cons 'ATOM c))
	  (if (eq (car e) 'quote)
	      (cons 'LDC (cons (car (cdr e)) c))
    ;; 2 args
	    (if (eq (car e) 'cons)
		(comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'CONS c)))
	      (if (eq (car e) 'eq)
		  (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'EQ c)))
		(if (eq (car e) 'leq)
		    (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'LEQ c)))
		  (if (eq (car e) 'add)
		      (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'ADD c)))
		    (if (eq (car e) 'sub)
			(comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'SUB c)))
		      (if (eq (car e) 'mul)
			  (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'MUL c)))
			(if (eq (car e) 'div)
			    (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'DIV c)))
			  (if (eq (car e) 'rem)
			      (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'REM c)))
			    ;; 3 args
			    (if (eq (car e) 'if)
				((lambda (cont-t cont-f)
				   (comp (car (cdr e)) n
					 (cons 'SEL (cons cont-t (cons cont-f c)))))
				 (comp (car (cdr (cdr e))) n '(JOIN))
				 (comp (car (cdr (cdr (cdr e)))) n '(JOIN)))
			      ;; many args
			      (if (eq (car e) 'lambda)
				  (cons 'LDF
					(cons
					 (cons (car (cdr e))
					       (comp (car (cdr (cdr e))) n '(RTN)))
					 c)
					)
				(if (eq (car e) 'let)
				    (cons 'DUM
					  (secd-comp--list
					   (car (cdr e))
					   n
					   (cons 'LDF (cons (cons (secd-comp--vars (car (cdr e))) (comp (car (cdr (cdr e))) n '(RTN))) (cons 'RAP c)))))
				  ;; Rest has to be an application
				  (secd-comp--args
				   (cdr e)
				   n
				   (comp (car e) n (cons 'AP c)))
				  ;; Done
				  )
				)
			      )
    )))))))))))))
  )

(defun secd-comp--vars (elist)
  "Returns list of variables in `let' records to be passed as arguments to the record expression."
  (if (eq elist nil) nil
    (cons (car (car elist)) (secd-comp--vars (cdr elist)))))

(defun secd-comp--list (elist n c)
  "Compiles a list of `let' record statements in order stated."
  (insert (format "\t--- comp--list\n\te: %s\n\tn: %s\n\tc: %s\n" elist n c))
  
  (if (eq elist nil) c
    (secd-comp--list
     (cdr elist)
     n
     (comp (car (cdr (car elist))) n c)
    )
    )
  )

(defun secd-comp--args (elist n c)
  "Compiles a list of expressions in order stated (in application forms)."
  (insert (format "\t--- comp--args\n\te: %s\n\tn: %s\n\tc: %s\n" elist n c))
  
  (if (eq elist nil) c
    (secd-comp--args
     (cdr elist)
     n
     (comp (car elist) n c)
    )
    )
  )

(provide 'secd-comp)
