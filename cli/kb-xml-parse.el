;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(require 'tree-widget)
(require 'svg-icon)

(defun kb-xml-parse-buffer (buffer-name)
  (interactive "bKB (XML) in buffer: ")
  (let ((parsed
	 (save-current-buffer
	   (set-buffer buffer-name)
	   (xml-parse-region (point-min) (point-max))
	   )))
    (xml->sexp (car parsed)))
  (cadr (with-current-buffer "*NXP-KB*"
	  (eval-buffer)
	  (goto-char (point-min))
	  (read (current-buffer)))
	)
  )

(defun kb-xml-parse-buffer-tree-widget (buffer-name)
  (interactive "bKB (XML) in buffer: ")
  (let ((parsed
	 (save-current-buffer
	   (set-buffer buffer-name)
	   (xml-parse-region (point-min) (point-max))
	   )))
    (with-current-buffer (get-buffer-create "*NXP-TREE-WIDGET*")
      (widget-create 'tree-widget (xml->tree-widget (car parsed)))
      )
    )
  )

(defun kb-xml--tag-text (elem root)
  (cond
   ((string= "ruleset" elem)
    (format "%s: %s" elem (cdr (assoc 'name (xml-node-attributes root)))))

   ((string= "rule" elem)
    (format "%s" elem))

   ((string= "hypothesis" elem)
    (format "%s: %s" elem (car (xml-node-children root))))

   ((string= "var" elem)
    (format "%s: %s" elem (car (xml-node-children root))))

   ((string= "const" elem)
    (format "%s: %s" elem (car (xml-node-children root))))

   ((string= "test" elem)
    (format "%s: %s" elem (cdr (assoc 'op (xml-node-attributes root)))))

   (t (format "%s" elem))
   )
  )
  
(defun xml->tree-widget (root)
  (cond ((null root) nil)
	((listp root)
	 (let ((elem (xml-node-name root))
	       (children
		(remove-if (function stringp) (xml-node-children root))))
	   `(tree-widget
	     :open t
	     :tag "kb"
	     :node (push-button
		    :tag ,(kb-xml--tag-text elem root)
		    :format "%[%t%]\n"
		    :xml-node ,root
		    :notify ,(lambda (widget &rest rest)
                               (message (format "%s" (widget-get widget :xml-node)))))
	     ,@(mapcar (lambda (x) (xml->tree-widget x)) children))))))


(defun xml->sexp (root)
  (let ((sexp-close
	 '(lambda ()
	    (with-current-buffer (get-buffer-create "*NXP-KB*")
	      (goto-char (point-max))
	      (insert (format "\n)\n"))
	      )
	    ))
	(sexp-cond
	 '(lambda (op var1 var2 const &optional negated)
	    (with-current-buffer (get-buffer-create "*NXP-KB*")
	      (goto-char (point-max))
	      (if negated (insert "(not "))
	      (if const
		  (insert (format "(%s  %s (quote %s)) " op var1 const))
		(insert (format "(%s %s %s) " op var1 var2)))
	      (if negated (insert ") "))
	      )
	    ))
	)

    (cond
     ((null root) nil)
     ((listp root)
      (let ((elem (xml-node-name root))
	    (children
	     (remove-if (function stringp) (xml-node-children root)))
	    )
	(cond
	 ((string= "ruleset" elem)
	  (with-current-buffer (get-buffer-create "*NXP-KB*")
	    (erase-buffer)
	    (goto-char (point-max))
	    (insert (format "(setq %s '("
			    (cdr (assoc 'name (xml-node-attributes root)))))
	    )
	  (mapcar #'(lambda (child) (xml->sexp child)) children)
	  (funcall sexp-close) (funcall sexp-close)
	  )

	 ((string= "rule" elem)
	  (with-current-buffer (get-buffer-create "*NXP-KB*")
	    (goto-char (point-max))
	    (insert (format "\n(rule "))
	    )
	  (mapcar #'(lambda (child) (xml->sexp child)) children)
	  (funcall sexp-close)
	  )

	 ((string= "hypothesis" elem)
	  (with-current-buffer (get-buffer-create "*NXP-KB*")
	    (goto-char (point-max))
	    (insert (format "%s " (car (xml-node-children root))))
	    )
	  )

	 ((string= "action" elem)
	  (with-current-buffer (get-buffer-create "*NXP-KB*")
	    (goto-char (point-max)) (insert (format "("))
	    )
	  (mapcar #'(lambda (child) (xml->sexp child)) children)
	  (funcall sexp-close)
	  )

	 ((string= "command" elem)
	  (let* (
		 (op    (cdr (assoc 'op (xml-node-attributes root))))
		 (var-node1   (car (xml-get-children root 'var)))
		 (var-node2   (cadr (xml-get-children root 'var)))
		 (const-node (car (xml-get-children root 'const)))
		 (var1   (car (xml-node-children var-node1)))
		 (var2   
		  (if var-node2 (car (xml-node-children var-node2)) nil))
		 (const
		  (if const-node (car (xml-node-children const-node)) nil))
		 )
	    (cond
	     ((string= "set-num" op) (funcall sexp-cond "set" var1 var2 const)
	     )
	     )
	    )
	  )

	 ((string= "condition" elem)
	  (with-current-buffer (get-buffer-create "*NXP-KB*")
	    (goto-char (point-max))
	    (insert (format "("))
	    )
	  (mapcar #'(lambda (child) (xml->sexp child)) children)
	  (funcall sexp-close)
	  )

	 ((string= "test" elem)
	  (let* (
		 (op    (cdr (assoc 'op (xml-node-attributes root))))
		 (var-node1   (car (xml-get-children root 'var)))
		 (var-node2   (cadr (xml-get-children root 'var)))
		 (const-node (car (xml-get-children root 'const)))
		 (var1   (car (xml-node-children var-node1)))
		 (var2   
		  (if var-node2 (car (xml-node-children var-node2)) nil))
		 (const
		  (if const-node (car (xml-node-children const-node)) nil))
		 )
	    ;; (with-current-buffer (get-buffer-create "*NXP-KB*")
	    ;;   (goto-char (point-max))
	    ;;   (insert (format ";; %s %s %s\n" op var const))
	    ;;   )
	    (cond
	     ((string= "lt" op) (funcall sexp-cond "leq" var1 var2 const)
	      )
	     ((string= "gt" op)	(funcall sexp-cond "leq" var1 var2 const t)
	      )
	     ((or (string= "in" op) (string="eq" op))
	      (funcall sexp-cond "eq" var1 var2 const)
	      )
	     ((or (string= "notin" op) (string="neq" op))
	      (funcall sexp-cond "eq" var1 var2 const t)
	      )
	     ((string= "yes" op)
	      (with-current-buffer (get-buffer-create "*NXP-KB*")
		(goto-char (point-max))
		(insert (format "(eq %s *T*)" var1))
		)
	      )
	     ((string= "no" op)
	      (with-current-buffer (get-buffer-create "*NXP-KB*")
		(goto-char (point-max))
		(insert (format "(eq %s *F*)" var1))
		)
	      )


	     )
	    )


	  )
	 )
	)
      )
     )
    )
  )

(provide 'kb-xml-parse)
