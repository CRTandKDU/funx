(defun secd-comp--kb-prompts (kb)
  (let (
	 (parse
	  (lambda (c p)
	    ;; (insert (format "%s\n" c))
	    (cond
	     ((atom c))
	     
	     ((and (or (eq (car c) 'eq) (eq (car c) 'leq) (eq (car c) 'set))
		   (atom (cadr c))
		   (listp (caddr c))
		   (eq (caaddr c) 'quote))
	      ;; (insert (format "%s\n\t%s\n\t%s\n\t%s\n\t%s\n"
	      ;; 		    c
	      ;; 		    (car c)
	      ;; 		    (cadr c)
	      ;; 		    (caaddr c)
	      ;; 		    (car (cdaddr c))))

	      (let ((var (cadr c)) (val (car (cdaddr c))))
		(if (assoc var p)
		    (unless (member val (cdr (assoc var p)))
		      (push val (cdr (assoc var p))))
		  (push (cons var (cons val nil)) p)))
	      ;; (insert (format "%s\n" p))
	      )

	     ((and (or (eq (car c) 'eq) (eq (car c) 'leq))
		   (atom (caddr c))
		   (listp (cadr c))
		   (eq (caadr c) 'quote))
	      ;; (insert (format "%s\n\t%s\n\t%s\n\t%s\n\t%s\n"
	      ;; 		    c
	      ;; 		    (car c)
	      ;; 		    (caddr c)
	      ;; 		    (caadr c)
	      ;; 		    (car (cdadr c))))
	      (let ((var (caddr c)) (val (car (cdadr c))))
		(if (assoc var p)
		    (unless (member val (cdr (assoc var p)))
		      (push val (cdr (assoc var p))))
		  (push (cons var (cons val nil)) p)))
	      ;; (insert (format "%s\n" p))
	      )
	     
	     ((and (eq (car c) 'in)
		   (atom (cadr c))
		   (listp (caddr c))
		   (eq (caaddr c) 'quote))
	      ;; (insert (format "%s\n\t%s\n\t%s\n\t%s\n\t%s\n"
	      ;; 		      c
	      ;; 		      (car c)
	      ;; 		      (cadr c)
	      ;; 		      (caaddr c)
	      ;; 		      (car (cdaddr c))))
	      (let ((var (cadr c)) (val (car (cdaddr c))))
		(dolist (v val)
		  (if (assoc var p)
		      (unless (member v (cdr (assoc var p)))
			(push v (cdr (assoc var p))))
		    (push (cons var (cons v nil)) p))))
	      ;; (insert (format "%s\n" p))
	      )
	      
	     )
	    p
	    )
	  )
	 )
    
    (let ((prompts nil))
      ;; Inspect LHS for basic `EQ', `LEQ' and `IN' operators on constants
      (dolist (rule kb)
	(let ((conds (caddr rule))
	      (actions (car (cdddr rule))))
	  ;; Parse LHS
	  (dolist (c conds)
	    (cond
	     ((atom c))
	     ((eq 'not (car c)) (setq prompts (funcall parse (cadr c) prompts)))
	     (t (setq prompts (funcall parse c prompts)))
	     )
	    )
	  ;; Parse RHS
	  (dolist (a actions)
	    (setq prompts (funcall parse a prompts)))
	  )
	)
      prompts
      )
    )
  )

(provide 'secd-comp-kb-prompts)
