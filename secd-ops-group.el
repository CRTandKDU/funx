(defconst secd--ops-true  '*T*)
(defconst secd--ops-false '*F*)
(defconst secd--ops-nil   nil)

(defun secd-car (s e c d)
  "CAR instruction, replaces top of stack with its car.
((a . b) . s) e (CAR . c) d --> (a . s) e c d
"
  (list (cons (car (car s)) (cdr s)) e (cdr c) d)
  )

(defun secd-cdr (s e c d)
  "CDR instruction, replaces top of stack with its cdr.
((a . b) . s) e (CDR . c) d --> (b . s) e c d
"
  (list (cons (cdr (car s)) (cdr s)) e (cdr c) d)
  )

(defun secd-atom (s e c d)
  "Tests atomicity of top of stack, using the implementation language primitive.
(a . s) e (ATOM . c) d --> (secd--ops-bool . s) e c d
according to whether `a' is an atom or not.
"
  (if (atom (car s))
      (list (cons secd--ops-true (cdr s)) e (cdr c) d)
    (list (cons secd--ops-false (cdr s)) e (cdr c) d))
  )

(defun secd-cons (s e c d)
  "Pops twice the stack and push back a cons.
(a b . s) e (CONS . c) d --> ((a . b) . s) e c d
"
  (list (cons (cons (car s) (car (cdr s))) (cdr (cdr s))) e (cdr c) d)
  )

(defun secd-eq (s e c d)
  "Pops twice the stack and push back true or false if or not equal.
(a b . s) e (EQ . c) d --> (secd--ops-bool . s) e c d
according to equality of `a' and `b'.
"
  (if (eq (car s) (car (cdr s)))
      (list (cons secd--ops-true (cdr (cdr s))) e (cdr c) d)
    (list (cons secd--ops-false (cdr (cdr s))) e (cdr c) d)
    )
  )

(defun secd-leq (s e c d)
  "Pops twice the stack and push back true or false if or not less.
(a b . s) e (LEQ . c) d --> (secd--ops-bool . s) e c d
according to equality of `a' and `b'.
"
  (if (<= (car s) (car (cdr s)))
      (list (cons secd--ops-true (cdr (cdr s))) e (cdr c) d)
    (list (cons secd--ops-false (cdr (cdr s))) e (cdr c) d)
    )
  )

(defun secd-sel (s e c d)
  "SEL Implements, together with JOIN, the if-then-else command.
({*T*|*F*} . s) e (SEL cT cF . c ) d --> s e c{T|F} (c . d)
"
  (let ((state
	 (list (cdr s)
	       e
	       (if (eq '*T* (car s)) (car (cdr c)) (car (cdr (cdr c))))
	       (cons (cdr (cdr (cdr c))) d )
	       ))
	)
;;    (insert (format "%s\n%s\n%s\n%s\n" (car state) (car (cdr state)) (car (cdr (cdr state))) (car (cdr (cdr (cdr state))))))
    state)
  )

(defun secd-join (s e c d)
  "JOIN Ends an if-then-else block.
s e (JOIN) (c . d) --> s e c d
"
  (list s e (car d) (cdr d))
  )


(provide 'secd-ops-group)

