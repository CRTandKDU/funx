;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun promise-pp (p)
  (let ((col1 (car p))
	(col2 (if (listp (cdr p)) '*UNKNOWN* (cdr p)))
	)
    (cond
     ;; True
     ((eq '*T* col2)
      (let ((cstr (format "#%02X%02X%02X" 0 255 0)))
	(insert
	 (propertize
	  (format "%-16s:\t%s" col1 col2)
	  'face `(foreground-color . ,cstr)))))
     ;; False
     ((eq '*F* col2)
      (let ((cstr (format "#%02X%02X%02X" 255 0 0)))
	(insert
	 (propertize
	  (format "%-16s:\t%s" col1 col2)
	  'face `(foreground-color . ,cstr)))))
     ;; Known
     ((null (eq '*UNKNOWN* col2))
      (let ((cstr (format "#%02X%02X%02X" 0 255 255)))
	(insert
	 (propertize
	  (format "%-16s:\t%s" col1 col2)
	  'face `(foreground-color . ,cstr)))))
     ;; Unknown
     (t (insert (format "%-16s:\t%s" col1 col2)))
     )
    )
  )
			    
(defun widget (env)
  (switch-to-buffer "*ENCY*")
  (erase-buffer)
  (let ((fsigns (cdr (assoc '*FWRD-SIGNS* env)))
	(wsigns (ewoc-create 'promise-pp (format "%19s" "Signs") "Footer"))
	(fhypos (cdr (assoc '*FWRD-RULES* env)))
	(whypos (ewoc-create 'promise-pp (format "%19s" "Hypos") "Footer"))
	)
    (let ((signs (-sort 'string< (mapcar 'car fsigns))))
      (dolist (sign signs)
	(ewoc-enter-last wsigns (assoc sign env))))
    (let ((hypos nil))
      (dolist (promise fhypos)
	(add-to-list 'hypos (cadr promise)))
      (dolist (hypo (-sort 'string< hypos))
	(ewoc-enter-last whypos (assoc hypo env)))
      )
    (list wsigns whypos)
    )
  )

(defun update-widget (ewoc var val)
  (let ((n 0))
    (while
	(and (ewoc-nth ewoc n)
	     (null (equal var (car (ewoc-data (ewoc-nth ewoc n))))))
      (setq n (1+ n)))
    (let ((node (ewoc-nth ewoc n)))
      (when node
	(ewoc-set-data node (cons var val))
	(ewoc-invalidate ewoc node))
      )
    )
  )  

;; `widgets' is the global return by the call to widget
(add-hook 'secd-env-update-hook
	  '(lambda (var val)
	     (dolist (w widgets)
	       (update-widget w var val))))

(remove-hook 'secd-env-update-hook
	  '(lambda (var val)
	     (dolist (w widgets)
	       (update-widget w var val))))

	    

(dolist (w widgets) (ewoc-refresh w))
  
