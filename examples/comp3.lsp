(let
    ((secd-comp--comp
      (lambda (e n c)
	(if (atom e) (cons 'LD (cons e c))
	  ;; 1 arg
	  (if (eq (car e) 'car)
	      (secd-comp--comp (car (cdr e)) n (cons 'CAR c))
	    (if (eq (car e) 'cdr)
		(secd-comp--comp (car (cdr e)) n (cons 'CDR c))
	      (if (eq (car e) 'atom)
		  (secd-comp--comp (car (cdr e)) n (cons 'ATOM c))
		(if (eq (car e) 'quote)
		    (cons 'LDC (cons (car (cdr e)) c))
		  ;; 2 args
		  (if (eq (car e) 'cons)
		      (secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'CONS c)))
		    (if (eq (car e) 'eq)
			(secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'EQ c)))
		      (if (eq (car e) 'leq)
			  (secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'LEQ c)))
			(if (eq (car e) 'add)
			    (secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'ADD c)))
			  (if (eq (car e) 'sub)
			      (secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'SUB c)))
			    (if (eq (car e) 'mul)
				(secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'MUL c)))
			      (if (eq (car e) 'div)
				  (secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'DIV c)))
				(if (eq (car e) 'rem)
				    (secd-comp--comp (car (cdr (cdr e))) n (secd-comp--comp (car (cdr e)) n (cons 'REM c)))
				  ;; 3 args
				  (if (eq (car e) 'if)
				      ((lambda (cont-t cont-f)
					 (secd-comp--comp (car (cdr e)) n
							  (cons 'SEL (cons cont-t (cons cont-f c)))))
				       (secd-comp--comp (car (cdr (cdr e))) n '(JOIN))
				       (secd-comp--comp (car (cdr (cdr (cdr e)))) n '(JOIN)))
				    ;; many args
				    (if (eq (car e) 'lambda)
					(cons 'LDF
					      (cons
					       (cons (car (cdr e))
						     (secd-comp--comp (car (cdr (cdr e))) n '(RTN)))
					       c)
					      )
				      (if (eq (car e) 'let)
					  (cons 'DUM
						(secd-comp--list
						 (car (cdr e))
						 n
						 (cons 'LDF (cons (cons (secd-comp--vars (car (cdr e))) (secd-comp--comp (car (cdr (cdr e))) n '(RTN))) (cons 'RAP c)))))
					;; Rest has to be an application
					(secd-comp--args
					 (cdr e)
					 n
					 (secd-comp--comp (car e) n (cons 'AP c)))
					;; Done
					)
				      )
				    )
				  )))))))))))))
	))
     (secd-comp--list
      (lambda (elist n c)
	(if (eq elist nil) c
	  (secd-comp--list (cdr elist) n
			   (secd-comp--comp (car (cdr (car elist))) n c)
			   ))))
     (secd-comp--vars
      (lambda (elist)
	(if (eq elist nil) nil
	  (cons (car (car elist)) (secd-comp--vars (cdr elist))))))
     )
  (secd-comp--comp '(mul (quote 2) (quote 3)) nil nil))
