;; Experimental client to the NXP architecture
;; Encyclopedia buffer, using ewocs. (Ewoc means “Emacs’s Widget for Object Collections”)
(require 'secd-comp-kb)

(defun nxp-ency--promise-pp (p)
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
     ;; Current
     ((eq col1 (car (car (cdr (assoc 'QUESTION session)))))
      (let ((cstr (format "#%02X%02X%02X" 255 153 0)))
	(insert
	 (propertize
	  (format "%-16s:\t%s" col1 col2)
	  'face `(foreground-color . ,cstr)))))
     ;; Unknown
     (t (insert (format "%-16s:\t%s" col1 col2)))
     )
    )
  )

(defun nxp-ency--kill-and-exit ()
  "Exits."
  (interactive)
  (kill-buffer nil)
  )

(defun nxp-ency--reset ()
  "Reset session by reinstalling promises in the environment"
  (interactive)
  (let ((new (copy-tree (cdr (assoc 'FASKB session)))))
    (setq session (assq-delete-all 'QUESTION session))
    (setcdr (assoc 'ENVIRONMENT session) new )
    (nxp-ency--widgets-update (cdr (assoc 'ENCY session)) new)
    )
  ;; (dolist (w (cdr (assoc 'ENCY session))) (ewoc-refresh w))
  )

;; session global has to be defined at this point
(defun nxp-ency--knowcess ()
  "Knowcess hypo at point."
  (interactive)
  (let ((node (ewoc-locate (cadr (cdr (assoc 'ENCY session))))))
    (save-current-buffer
      (set-buffer (get-buffer-create "*NXP-SESSION*"))
      (erase-buffer)
      (insert (format "-- NEW SESSION:\ns:%s\n" (ewoc-data node)))
      )
    (let ((question
	   (secd-comp--kb-knowcess
	    (cdr (assoc 'ENVIRONMENT session))
	    (cons (car (ewoc-data node)) nil))))
      (if (assoc 'QUESTION session)
	  (setcdr (assoc 'QUESTION session) question)
	(push (cons 'QUESTION question) session))
      (dolist (w (cdr (assoc 'ENCY session)))
	(nxp-ency-update-widget w (caar question)))
      )
    )
  )

(defun nxp-ency--answer (val)
  "Answers pending question from the Encyclopedia buffer."
  (interactive
   (list
    (let ((prompts (cdr (assoc secd--kb-prompts
			       (cdr (assoc 'ENVIRONMENT session)))))
	  (var (car (car (cdr (assoc 'QUESTION session))))))
      ;; (read-from-minibuffer
      ;;  (format "What is the value of %s: " var) nil nil t)
      (car (read-from-string
	    (completing-read
	     (format "What is the value of %s: " var)
	     (mapcar #'(lambda (x) (format "%s" x))
		     (cdr (assoc var prompts))))))
      )
    )
   )
  (save-current-buffer
    (let ((prompts (cdr (assoc secd--kb-prompts
			       (cdr (assoc 'ENVIRONMENT session)))))
	  (var (car (car (cdr (assoc 'QUESTION session))))))
      (set-buffer (get-buffer-create "*NXP-SESSION*"))
      (goto-char (point-max))
      (insert (format "KNOWN VALUES> %s\n" (cdr (assoc var prompts))))
      (insert (format "ANSWER> %s\n" val))
      )
    )
  (if (assoc 'QUESTION session)
      (let ((question
	     (secd-answer (cdr (assoc 'QUESTION session)) val t)))
	(if (assoc 'QUESTION session)
	    (setcdr (assoc 'QUESTION session) question)
	  (push (cons 'QUESTION question) session))
	(dolist (w (cdr (assoc 'ENCY session)))
	  (nxp-ency-update-widget w (caar question)))
	)
    )
  )

(setq nxp-ency-mode-map
      (let ((m (make-sparse-keymap)))
        (suppress-keymap m t)
        (define-key m "r" 'nxp-ency--reset)
        (define-key m "k" 'nxp-ency--knowcess)
        (define-key m "a" 'nxp-ency--answer)
        (define-key m "q" 'nxp-ency--kill-and-exit)
        m))
			    
(defun nxp-ency-init (env)
  (switch-to-buffer "*NXP-ENCY*")
  (kill-all-local-variables)
  (setq major-mode 'nxp-ency-mode mode-name "NXP Encyclopedia mode")
  (use-local-map nxp-ency-mode-map)
  (erase-buffer)
  (let ((fsigns
	 (let ((-compare-fn (lambda (x y) (equal (car x) (car y)))))
	   (-union (cdr (assoc secd--kb-forward-chaining-signs env))
		   (cdr (assoc secd--kb-backward-chaining-signs env)))))
	(wsigns (ewoc-create 'nxp-ency--promise-pp (format "%19s" "Signs")
			     (substitute-command-keys
                              "\n\\{nxp-ency-mode-map}")
			     ))
	(fhypos (cdr (assoc secd--kb-forward-chaining-rules env)))
	(whypos (ewoc-create 'nxp-ency--promise-pp (format "%19s" "Hypos") "Footer"))
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

(defun nxp-ency--widgets-update (wgts env)
  (dolist (w wgts)
    (let ((cur (ewoc-nth w 0)))
      (ewoc-set-data cur (assoc (car (ewoc-data cur)) env))
      ;; (save-current-buffer
      ;; 	(set-buffer (get-buffer-create "*NXP-SESSION*"))
      ;; 	(insert (format "UPDATE WGT: %s\n" (ewoc-data cur)))
      ;; 	)
      (while (setq cur (ewoc-next w cur))
	(ewoc-set-data cur (assoc (car (ewoc-data cur)) env))
	;; (save-current-buffer
	;;   (set-buffer (get-buffer-create "*NXP-SESSION*"))
	;;   (insert (format "UPDATE WGT: %s\n" (ewoc-data cur)))
	;;   )
	)
      (ewoc-refresh w)
      )
    )
  )
  
      
(defun nxp-ency-update-widget (ewoc var &optional val)
  ;; (save-current-buffer
  ;;   (set-buffer (get-buffer-create "*NXP-SESSION*"))
  ;;   (insert (format "UPDATE WGT: %s %s\n" var val))
  ;;   )
  (let ((n 0))
    (while
	(and (ewoc-nth ewoc n)
	     (null (equal var (car (ewoc-data (ewoc-nth ewoc n))))))
      (setq n (1+ n)))
    (let ((node (ewoc-nth ewoc n)))
      (when node
	(if val
	    (ewoc-set-data node (cons var val)))
	(ewoc-invalidate ewoc node))
      )
    )
  )

(defun nxp-ency--hook (var val &optional state)
  (dolist (w widgets) (nxp-ency-update-widget w var val)))

(defun nxp-ency--stop-hook (state)
  (save-current-buffer
    (set-buffer (get-buffer-create "*NXP-SESSION*"))
    (goto-char (point-max))
    (insert (format "Session closed.\n%s\n" (car state)))
    )
  )

(defun nxp-session (kb)
  "The NXP session alist contains all runtime information."
  (let ((session nil) (env nil))
    ;; Compile kb to environment
    (setq env (secd-comp--kb2env kb))
    ;; Init widgets and callbacks (as Emacs hooks)
    (setq widgets (nxp-ency-init env))
    (remove-hook 'secd-env-update-hook 'nxp-ency--hook)
    (add-hook 'secd-env-update-hook 'nxp-ency--hook)
    (remove-hook 'secd-exec-stop-hook 'nxp-ency--stop-hook)
    (add-hook 'secd-exec-stop-hook 'nxp-ency--stop-hook)
    ;; Builds alist
    (push (cons 'KB kb) session) ;; KB source code 
    (push (cons 'FASKB (copy-tree env)) session) ;; KB compiled code, immutable
    (push (cons 'ENVIRONMENT env) session) ;; Initial environment 
    (push (cons 'ENCY widgets) session) ;; Encyclopedia ewoc-based GUI
    session
    )
  )
    

;; `widgets' is the global return by the call to widget
;; (remove-hook 'secd-env-update-hook
;; 	  '(lambda (var val &optional state)
;; 	     (dolist (w widgets)
;; 	       (update-widget w var val))))

;; (add-hook 'secd-env-update-hook
;; 	  '(lambda (var val &optional state)
;; 	     (dolist (w widgets)
;; 	       (update-widget w var val))))



;;(dolist (w widgets) (ewoc-refresh w))
(provide 'nxp-ency)

