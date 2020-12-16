(defun secd-add (s e c d)
  "ADD Addition.
(a b . s) e (ADD . c) d --> ( b+a . s) e c d
"
  (list
   (cons (+ (car (cdr s)) (car s)) (cdr (cdr s)))
   e (cdr c) d)
  )

(defun secd-sub (s e c d)
  "SUB Subtraction.
(a b . s) e (SUB . c) d --> ( b-a . s) e c d
"
  (list
   (cons (- (car (cdr s)) (car s)) (cdr (cdr s)))
   e (cdr c) d)
  )

(defun secd-mul (s e c d)
  "MUL Multiplication.
(a b . s) e (MUL . c) d --> ( b*a . s) e c d
"
  (list
   (cons (* (car (cdr s)) (car s)) (cdr (cdr s)))
   e (cdr c) d)
  )

(defun secd-div (s e c d)
  "DIV Division.
(a b . s) e (DIV . c) d --> ( b/a . s) e c d
"
  (list
   (cons (/ (car (cdr s)) (car s)) (cdr (cdr s)))
   e (cdr c) d)
  )

(defun secd-rem (s e c d)
  "REM Modulo.
(a b . s) e (REM . c) d --> ( b mod a . s) e c d
"
  (list
   (cons (% (car (cdr s)) (car s)) (cdr (cdr s)))
   e (cdr c) d)
  )


(provide 'secd-arith-group)

