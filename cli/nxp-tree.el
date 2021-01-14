;; Experimental client to the NXP architecture
;; Tree buffer, using `tree-widget'
(require 'secd-comp-kb)
(require 'svg-icon)

(defconst cli-nxp-tree-icon-rhs
  (propertize "--" 'display (svg-icon "octicons" "arrow-right")))
(defvar cli-nxp-tree-buffer "*NXP-TREE*")

(defun nxp-tree--subgoal (c env session)
  "If condition `c' is a subgoal, expand as a tree-widget. (In `yes' and `no' operators only.)"
  (let
      ((subgoal (cadr (assoc c (cdr (assoc secd--kb-cond-source env))))))
    (cond
     ((atom subgoal) (nxp-tree-hypo--children subgoal session))
     ((and (eq 'not (car subgoal)) (atom (cadr subgoal)))
      (nxp-tree-hypo--children (cadr subgoal) session))
     (t nil)
     )
    )
  )

(defun nxp-tree--rule (rule env session)
  "Tree representation of a rule. Disassemble control list to extract conditions and actions."
  (let* ((ccode-conds (secd-comp-lhs rule env))
	 (ccode-actions (secd-comp-rhs rule env)))

    (append
     (mapcar #'(lambda (c)
		(widget-convert
		 'tree-widget
		 :open t
		 :tag (nxp-tree--tag
		       c
		       (cdr (assoc 'ENVIRONMENT session))
		       (cadr (assoc c (cdr (assoc secd--kb-cond-source env)))))
		 :args (nxp-tree--subgoal c env session)
		 ))
	    ccode-conds)
     (mapcar #'(lambda (c)
		(widget-convert
		 'tree-widget
		 :open t
		 :tag  (let ((tag (format "%s %s: %s"
					  cli-nxp-tree-icon-rhs
					  c
					  (cadr (assoc c (cdr (assoc secd--kb-cond-source env)))))))
			 (if (eq '*T* (cdr (assoc rule (cdr (assoc 'ENVIRONMENT session)))))
			     (let ((cstr (format "#%02X%02X%02X" 0 255 0)))
			       (propertize tag 'face `(foreground-color . ,cstr)))
			   tag))

		 :args nil
		 )
		)
	     ccode-actions)
     )
    )
  )

(defun nxp-tree--tag (promise rt-env &optional str)
  (let ((val (cdr (assoc promise rt-env)))
	(tag (if str (format "%s: %s" promise str) (format "%s" promise))))
    (if (listp val) tag
      (cond
       ((eq '*T* val)
	(let ((cstr (format "#%02X%02X%02X" 0 255 0)))
	  (propertize tag 'face `(foreground-color . ,cstr))))
       
       ((eq '*F* val)
	(let ((cstr (format "#%02X%02X%02X" 255 0 0)))
	  (propertize tag 'face `(foreground-color . ,cstr))))

       (t tag)
       )
      )
    )
  )

(defun nxp-tree-hypo--children (h session)
  "List of tree representations of children nodes of hypothesis `h'"
  (let* ((env    (cdr (assoc 'FASKB session)))
	 (rt-env (cdr (assoc 'ENVIRONMENT session)))
	 (r-to-h (cdr (assoc secd--kb-forward-chaining-rules env)))
	 (rules
	  (mapcar 'car
		  (-filter (lambda (edge) (equal h (cadr edge))) r-to-h)))
	 )
    (mapcar
     #'(lambda (rule)
	 (widget-convert
	  'tree-widget
	  :open t
	  :tag (nxp-tree--tag rule rt-env)
	  :args (nxp-tree--rule rule env session)
	  ))
     rules)
    )
  )


(defun nxp-tree-hypo (h session)
  (with-current-buffer (get-buffer-create cli-nxp-tree-buffer)
    (erase-buffer)
    (widget-create 'tree-widget
		   :open t
		   :tag (format "%s" h)
		   :args (list (nxp-tree-hypo--children h session)))
    (widget-setup)
    )
  )

;; Plug-in into an ewoc
(defun nxp-tree--pp (h)
  (widget-create 'tree-widget
		 :open t
		 :tag (nxp-tree--tag h (cdr (assoc 'ENVIRONMENT session)))
		 :args (nxp-tree-hypo--children h session))
  )

(defun nxp-tree-init (h session)
  "Initializes a signle data node ewoc with hypo and returns the ewoc."
  (with-current-buffer (get-buffer-create cli-nxp-tree-buffer)
    (erase-buffer)
    (let ((ewoc (ewoc-create 'nxp-tree--pp "Header" "Footer")))
      (ewoc-enter-last ewoc h)
      ewoc
      )
    )
  )

(provide 'nxp-tree)
