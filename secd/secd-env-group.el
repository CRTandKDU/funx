;; Environment management : implementation as an alist

(defun secd-env--locate (env var)
  "Implementation of LD mnemonic as a simple alist lookup."
  (cdr (assoc var env))
  )

(defun secd-env--update (env promise val)
  "Updates `promise' in environment `env' with `val' or creates binding."
  (if (assoc promise env)
      (setcdr (assoc promise env) val)
    (push (cons promise val) env))
  env
  )

(defun secd-env--rupdate (env clist val)
  "Replaces `clist' in first found promise in `env' with `val', to implement the UPD mnemonic. Returns update or nil if not found."
  (if (rassoc clist env)
      (setcdr (rassoc clist env) val))
  )

(defun secd-env--rupdate-promise (promise val)
  "Updates `promise' if not nil with `val', to implement the UPD mnemonic. Returns promise or nil."
  (if promise (setcdr promise val))
  )

(defun secd-env--push (env alist)
  "Pushes alist into environment, returning extended environment. Used in AP."
  (append alist env)
  )


(provide 'secd-env-group)
