(require 'secd-env-group)

(defun secd-ldc (s e c d)
  "LDC Loads a constant on top of the stack.
s e (LDC x . c) d --> (x . s) e c d
"
  (list (cons (car (cdr c)) s) e (cdr (cdr c)) d)
  )

;; Environment as a cons of 2 lists of list of atoms (n . v)
;; Environment as an alist
(defun secd-ld (s e c d)
  "LD Loads variable value on stack.
s e (LD a . c) d --> (locate(a,e) . s) e c d 
"
  (list (cons (secd-env--locate e (car (cdr c)))  s) e (cdr (cdr c)) d)
  )

(defun secd-ldf (s e c d)
  "LDF Loads a function implementation.
s e (LDF n c' . c) d --> (( n . (c' . e)) . s) e c d
"
  (list
   (let (( news (cons (cons (car (car (cdr c))) (cons (cdr (car (cdr c))) e)) s)  ))
     ;; (insert (format "Exit LDF: %s\n" news))
     news)
   e
   (cdr (cdr c))
   d)
  )

(provide 'secd-ld-group)

