;; Environment management : implementation as an alist
;; Installs forward-chaining using hooks in Emacs-Lisp

(defvar secd-env-update-hook nil)

(defun secd-env--locate (env var &optional state)
  "Implementation of LD mnemonic as a simple alist lookup."
  (cdr (assoc var env))
  )

(defun secd-env--update (env promise val &optional state)
  "Updates `promise' in environment `env' with `val' or creates binding."
  (if (assoc promise env)
      (setcdr (assoc promise env) val)
    (push (cons promise val) env))
  (run-hook-with-args 'secd-env-update-hook promise val state)
  env
  )

(defun secd-env--rupdate (env clist val &optional state)
  "Replaces `clist' in first found promise in `env' with `val', to implement the UPD mnemonic. Returns update or nil if not found."
  (let ((promise (rassoc clist env)))
    (cond
     (promise
      (setcdr promise val)
      (run-hook-with-args 'secd-env-update-hook (car promise) val state)
      env)
     (t nil)
     )
    )
  )

(defun secd-env--rupdate-promise (promise val &optional state)
  "Updates `promise' if not nil with `val', to implement the UPD mnemonic. Returns promise or nil."
  (if promise (setcdr promise val))
  (if promise (run-hook-with-args 'secd-env-update-hook promise val state))
  )

(defun secd-env--push (env alist &optional state)
  "Pushes alist into environment, returning extended environment. Used in AP."
  (append alist env)
  )


(provide 'secd-env-group)
