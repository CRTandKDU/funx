(require 'secd-env-group)


(defun secd-ask (s e c d)
  "ASK for variable pushed on stack. Pauses evaluation.
s e (ASK v . c) d --> (v . s) e c d
"
  ;; Trace
  (save-current-buffer
    (set-buffer (get-buffer-create "*SECD*"))
    (insert (format "What is the value of %s?\n" (car (cdr c))))
    )
  (save-current-buffer
    (set-buffer (get-buffer-create "*NXP-SESSION*"))
    (insert (format "What is the value of %s?\n" (car (cdr c))))
    )
  (list (cons (car (cdr c)) s) e (cdr (cdr c)) d)
  )

;; 1. Could alternatively leave `val' on top of stack.
;; 2. Revisit with delay/force. ASK could replace LD completely?
(defun secd-answer (state val &optional resume)
  "Pops a variable from stack. Assigns value `val' and updates stack and env.
s e c d --> cdr(s) (push(cons(car(s),val),e) c d
"
  (let* ((e (secd--e state))
	 (promise (car (secd--s state)))
;;	 (entry (assoc (car (secd--s state)) e))
	 )
    ;; (if entry
    ;; 	(setcdr entry val)
    ;;   ;; Should not be in the situation where variable is not a
    ;;   ;; promise in the environment. The new binding will be lost at
    ;;   ;; next UPD.
    ;;   (setq e (cons (cons (car (secd--s state)) val) e))
    ;;   )
    (secd-env--update e promise val state)
    (if resume
	(secd-cycle (cons val (cdr (secd--s state)))
	      e
	      (secd--c state)
	      (secd--d state)
	      )
      (list (cons val (cdr (secd--s state)))
	    e
	    (secd--c state)
	    (secd--d state)
	    )
      )
    )
  )


(provide 'secd-io-group)

