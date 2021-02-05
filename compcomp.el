(require 'ox-texinfo)
(normal-top-level-add-subdirs-to-load-path)
(require 'secd-exec)
(require 'secd-comp-kb)
(require 'nxp-ency)
(require 'kb-xml-parse)

;;; STEP I
(defun secd-comp--vars (elist)
  "Returns list of variables in `let' records to be passed as arguments to the record expression."
  (if (eq elist nil) nil
    (cons (car (car elist)) (secd-comp--vars (cdr elist)))))

(secd-comp--vars '((A . 1) (B . 2) (C . 3)))
(A B C)

(setq fasl
      (secd-comp--comp
       '(let
	    ((secd-comp--vars
	      (lambda (elist)
		(if (eq elist nil) nil
		  (cons (car (car elist)) (secd-comp--vars (cdr elist))))))
	     )
	  (secd-comp--vars '((A 1) (B 2))))
       nil
       '(STOP)
       )
      )

(secd-cycle nil nil fasl nil)

;; STEP II
(defun secd-comp--stub (e n c) c)
(defun secd-comp--list (elist n c)
  "Compiles a list of `let' record statements in order stated."
  (insert (format "\t--- comp--list\n\te: %s\n\tn: %s\n\tc: %s\n" elist n c))
  
  (if (eq elist nil) c
    (secd-comp--list
     (cdr elist)
     n
     (secd-comp--stub (car (cdr (car elist))) n c)
    )
    )
  )

(secd-comp--list '((A AA) (B BB) (C CC)) nil '(STOP))


(setq fasl
      (secd-comp--comp
       '(let
	    ((secd-comp--comp (lambda (e n c) c))
	     (secd-comp--list
	      (lambda (elist n c)
		(if (eq elist nil) c
		  (secd-comp--list (cdr elist) n
				   (secd-comp--comp (car (cdr (car elist))) n c)
				   )
		  )))
	     (secd-comp--vars
	      (lambda (elist)
		(if (eq elist nil) nil
		  (cons (car (car elist)) (secd-comp--vars (cdr elist))))))
	     )
	  (secd-comp--list '((A 1) (B 2))))
       nil
       '(STOP)
       )
      )

(secd-cycle nil nil fasl nil)


	 
