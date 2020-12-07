(defun secd--zip-rest (names values)
  "Analog to the Python `zip' function; creates an alist of (name . value) in car of result. Residual values are returned in the cdr of the result.
"

  (if (null names) (cons nil values)
    (let ((rest (secd--zip-rest (cdr names) (cdr values))))
      (cons
       (cons (cons (car names) (car values)) (car rest))
       (cdr rest))
    )
    )
  )

;;; Function application.
;;; Used both for `let' and `funcall' blocks. Expect closures on the stack.
(defun secd-ap (s e c d)
  "AP Application. (See also: LDF)
((n . (c' . e')) v . s) e (AP . c) d --> NIL ((n . v) . e') c' (s e c . d)

Note: Differs from Henderson's implementation. The mnemonic LDF
has a list of bound variables as first argument, `n' in the above
transition. This list is used to zip names and values popped from
stack, here `v', to the front of the environment alist.
"
  ;; (insert (format "Enter AP: s = %s\n" s))
  ;; (insert (format "Enter AP: e = %s\n" e))
  (let* (
	 (s-e (secd--zip-rest (car (car s)) (cdr s)))
	 (ap-s (cdr s-e))
	 (ap-e (if (null (car s-e)) e (append (car s-e) e) ))
	 )
    ;; (insert (format "Exit  AP: e = %s\n" ap-e))
    ;; (insert (format "Exit  AP: c = %s\n" (car (cdr (car s)))))
    ;; (insert (format "Exit  AP: d = %s\n" (cons ap-s (cons e (cons  (cdr c) d)))
    ;; 		    ))
    (list
     nil
     ap-e
     (car (cdr (car s)))
     (cons ap-s (cons e (cons (cdr c) d)))
     )
    )
  )

(defun secd-rtn (s e c d)
  "RTN Returns from application with result on top of stack (singleton).
(r) e' (RTN) (s e c . d) --> (r . s) e c d
"
  ;; Could check for singleton?
  (list
   (cons (car s) (car d))
   ;; (cons s (car d))
   (car (cdr d))
   (car (cdr (cdr d)))
   (cdr (cdr (cdr d)))
   )
  )

;;; Recursive blocks
;;; Used for `letrec' where variables are bound to `lambda' expressions.
(defun secd-dum (s e c d)
  "DUM Creates a special environment for recursive blocks `letrec'. (See also RAP.)"
  ;; Uses nil as the special entry \omega
  (list s (cons 'OMEGA e) (cdr c) d)
  )

;; (defun secd-rap (s e c d)
;;   "RAP Similar to AP but uses `rplaca' to replace the `nil' (omega) set by DUM.
;; ((n . (c' . e')) v . s) (OMEGA . e) (RAP . c) d --> NIL rplaca(e', v) c' (s e c . d)
;; "
;;   ;; (insert (format "Enter  RAP: caar s, cdr s \n\t%s\n\t%s\n" (car (car s)) (cdr s)))
;;   (let* (;; Preserves entry environment as the argument `e' is edited in place
;; 	 (new-e (cdr (copy-tree e)))
;; 	 (e-s (secd--zip-rest (car (car s)) (cdr s)))
;; 	 (eprime (car e-s))
;; 	 (rap-s  (cdr e-s))
;; 	 ;; impacts `e' which shares car with `eprime'
;; 	 (rap-e (dolist (closure eprime eprime)
;; 		  (setcdr
;; 		   (cdr (cdr (cdr closure)))
;; 		   (append (cdr eprime) (cdr (cdr (cdr (cdr closure))))) )
;; 		  (setcar
;; 		   (cdr (cdr (cdr closure)))
;; 		   (car eprime))
;; 		  ))
;; 	 )

;;     (save-current-buffer
;;       (set-buffer (get-buffer-create "*SECD*"))
;;       (insert (format "RAP e1: %s\n" (car e-s)))
;;       (insert (format "RAP s1: %s\n" (cdr e-s)))
;;       (insert (format "RAP  2: %s\n" e))
;;       (insert (format "\n"))
;;       )

;;     (list
;;      nil
;;      e ;; not eprime, we need the original emcompassing env for globals
;;      (car (cdr (car s)))     
;;      (cons rap-s (cons new-e (cons (cdr c) d)))
;;      )
;;     )
;;   )

(defun secd-rap (s e c d)
  "RAP Similar to AP but uses `rplaca' to replace the `nil' (omega) set by DUM.
((n . (c' . e')) v . s) (OMEGA . e) (RAP . c) d --> NIL rplaca(e', v) c' (s e c . d)
"
  ;; (insert (format "Enter  RAP: caar s, cdr s \n\t%s\n\t%s\n" (car (car s)) (cdr s)))
  (let* (;; Preserves entry environment as the argument `e' is edited in place
	 (e-orig (copy-tree (cdr e)))
	 (e-s (secd--zip-rest (car (car s)) (cdr s)))
	 (e-prime  (car e-s))
	 (s-prime  (cdr e-s))
	 )

    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD*"))
      (insert (format "RAP e': %s\n" (car e-s)))
      (insert (format "RAP s': %s\n" (cdr e-s)))
      (insert (format "RAP e : %s\n" e))
      (insert (format "\n"))
      )

    (setcar e e-prime)

    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD*"))
      (insert (format "RAP e': %s\n" (car e-s)))
      (insert (format "RAP s': %s\n" (cdr e-s)))
      (insert (format "RAP e : %s\n" e))
      (insert (format "\n"))
      )

    (list
     nil
     e-prime
     (car (cdr (car s)))     
     (cons s-prime (cons e-orig (cons (cdr c) d)))
     )
    )
  )


(provide 'secd-fun-group)

