;;; Compilers
;;; Strict Compiler
(defun secd-comp--comp (e n c)
  "Compiles strictly expression e, with names n and continuation c."
  (insert (format "--\ne: %s\nn: %s\nc: %s\n" e n c))
  ;; 0 arg
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
     (secd-comp--comp (car (cdr (car elist))) n c)
    )
    )
  )

(defun secd-comp--args (elist n c)
  "Compiles a list of expressions in order stated (in application forms)."
  ;; (insert (format "\t--- comp--args\n\te: %s\n\tn: %s\n\tc: %s\n" elist n c))
  
  (if (eq elist nil) c
    (secd-comp--args
     (cdr elist)
     n
     (secd-comp--comp (car elist) n c)
    )
    )
  )

;;; Commands
(defun secd-compile (file)
  "Compiles source code in `file.lsp' into control list file `file.fasl'" 
  (let* ((source (with-temp-buffer
		  (insert-file-contents file)
		  (buffer-string)))
	)
    (with-temp-file (format "%s.fasl" (file-name-sans-extension file))
      (insert (format "(setq bytecode (secd-comp--comp '%s nil '(STOP)))" source))
      (let ((ignore (eval-buffer)))
	(erase-buffer)
	(insert (format "%s" bytecode))
	)
      )
    )
  )


(provide 'secd-comp)
