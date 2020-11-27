(defun secd-ask (s e c d)
  "ASK for variable pushed on stack.
s e (ASK v . c) d --> (v . s) e c d
"
  (list (cons (car (cdr c)) s) e (cdr (cdr c)) d)
  )

;; 1. Could alternatively leave `val' on top of stack.
;; 2. Revisit with delay/force. ASK could replace LD completely?
(defun secd-answer (state val)
  "Pops a variable from stack. Assigns value `val' and updates stack and env.
s e c d --> cdr(s) (push(cons(car(s),val),e) c d
"
  ;; See ASK mnemonics
  (list (cdr (secd--s state))
	(cons (cons (car (secd--s state)) val) (secd--e state))
	(secd--c state)
	(secd--d state)
	)
  )

(provide 'secd-io-group)

