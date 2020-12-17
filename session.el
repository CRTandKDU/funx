;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(push "C:/Users/jmc/Documents/code/funx" load-path)
(normal-top-level-add-subdirs-to-load-path)
(require 'secd-exec)



(setq a (secd-cycle nil '((x . 7)) '(LDC 14 LD x ASK y LDC 8 STOP) nil))What is the value of y?

;; (car (secd--s a))
;; (secd--e a)
;; (car (secd--c a))

;; (defun secd-answer (state val)
;;   (list (cdr (secd--s state))
;; 	(cons (cons (car (secd--s state)) val) (secd--e state))
;; 	(secd--c state)
;; 	(secd--d state)
;; 	)
;;   )

(setq b (secd-cycle nil '((x . UNKNOWN)) '(LDC 14 LD x LDC 8 STOP) nil))
(let ((state (secd-answer a 16)))
  (secd-cycle (secd--s state) (secd--e state) (secd--c state) (secd--d state))
  )

;; (let ((x 3)) (eq (cdr (funcall (lambda (x y) (cons x y)) 5 x)) 3))
;; --> t
(setq b
      (secd-cycle
       nil
       '((x . 3))
       '(LDC 5 LD x LDF (LD x LD y CONS RTN) AP (x y) CDR LDC 3 EQ STOP)
       nil))


(setq b1
      (secd-cycle
       nil
       '((x . 3))
       '(LDC 5 ASK z LDC 6 LDF (LD z LD y LD x CONS CONS RTN) AP (x y) STOP)
       nil))
(let ((state (secd-answer b1 12)))
  (secd-cycle (secd--s state) (secd--e state) (secd--c state) (secd--d state))
  )

nil,           y.5 | x.6 | z.12 | x.3
12 | 5 | 6 ,   y.5 | x.6 | z.12 | x.3
(12 . 5 ) | 6, y.5 | x.6 | z.12 | x.3
(12 5 . 6)

(let ((z 12)) (funcall (lambda (x y) (cons (cons x y) z)) 6 5))

;; Nested applications  (cons (cons 5 6) 3)
;; (funcall cons (funcall cons (5 6) 3))
(setq b1
      (secd-cycle
       nil
       '((x . 3))
       '(LDC 12 LD x LDC 5 LDC 6 LDF ((x y) . (LD y LD x CONS RTN)) AP LDF ((x y) . (LD y LD x CONS RTN)) AP STOP)
       nil))

(append '(1 2) '(3 4))

;; Constants as functions
(setq b2
      (secd-cycle
       nil
       '((x . 3))
       '(LDC 12 LD x LDC 5 LDC 6 LDF (() . (LDC 4 RTN)) AP STOP)
       nil))

(setq b3
      (secd-cycle
       nil
       '((z . 3))
       '(DUM
	 LDC 5
	 LDC 4
	 LDC 3
	 LDF ((x y) . (LD x LDC y CONS RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )

;; Application with global variable
(setq b4
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(LD b LDF ( (n) . (LD n LDC 1 SUB RTN) ) AP STOP)
       nil
       )
      )

;; IF-THEN
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(LD a LDC 12 EQ SEL (LDC 1 JOIN) (LDC 2 JOIN)  STOP)
       nil
       )
      )

;; Nested
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 3))
       '(LD a
	LDC 12
	EQ
	SEL
	(LD b LDC 4 LEQ SEL (LDC 1 JOIN) (LDC 3 JOIN) JOIN)
	(LDC 2 JOIN)
	STOP
	)
       nil
       )
      )

;; Factorial with alternate calls to same function
(setq b6
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(DUM
	 LDF ( (n) . (LD n LDC 1 EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD FAC AP LD n MUL JOIN) RTN) )
	 LDF ( (n) . (LD n LDC 1 EQ SEL (LDC 1 JOIN) (LD n LDC 1 SUB LD FBC AP LD n MUL JOIN) RTN) )
	 LDF ( (FBC FAC) . (LDC 4 LD FAC AP RTN) )
	 RAP
	 STOP)
       nil
       )
      )

;; Application
(setq b6
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 LDC 2
	 LDC 3
	 LDF ( (n z) . (LD z LD n ADD RTN) )
	 AP
	 STOP
	 )
       nil
       )
      )

;; Higher order function: function as an argument
(setq b7
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 DUM
	 LDF ( () . (LDC 3 RTN) )
	 LDF ( (THREE) . (LD a LD THREE AP ADD LD b LD THREE AP ADD CONS RTN) )
	 RAP
	 STOP
	 )
       nil
       )
      )

(insert (format "%s\n" b7))
((3) ((THREE nil (LDC 3 RTN) #2 (a . 12) (b . 5))) (STOP LDC 2 RTN) (nil ((a . 12) (b . 5)) (STOP)))

(setq b8
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 ;; (let ((x 4)) (+ x 6))
	 LDF (() . (LDC 4 RTN))
	 LDF ((CONST4) . (LD CONST4 AP LDC 6 ADD RTN))
	 AP
	 STOP
	 )
       nil
       )
      )

;; Testing atom v. list
(setq b9
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 LD nil
	 LDC 1
	 CONS
	 ATOM
	 STOP
	 )
       nil
       )
      )
(cdr (assoc 'H '((a . 12) (b . 5) (H . (LD a LDC 6 ADD UPD)) )))

;; Test promises
(setq b9
      (secd-cycle
       nil
       '((a . 12) (b . 5) (H . (LD a LDC 6 ADD UPD)) )
       '(
	 LD b
	 LDP H
	 AP0
	 ADD
	 LDE (LD a LDC 6 ADD UPD)
	 AP0
	 ADD
	 STOP
	 )
       nil
       )
      )

(setq b9
      (secd-cycle
       nil
       '((a . 12) (b . 5) (H . (LD a LDC 6 ADD UPD)) )
       '(
	 LDE (LD a LDC 6 ADD UPD)
STOP
	     AP0
	     STOP
	 )
       nil
       )
      )


;; Note that because of the use of `rassoc' the promise is also updated in H
(setq b9bis
      (secd-cycle
       nil
       '((a . 12) (b . 5) (H . (LD a LDC 6 ADD UPD)) )
       '(
	 LDC 1
		 ;; LDE (LD a LDC 6 ADD UPD)
		 LDE (LDC 6 LD a ADD UPD)
	 ;; LDP H
	 AP0
	 ADD
	 ;; LDP H
	 ;; AP0
	 ;; ADD
	 STOP
	 )
       nil
       )
      )

(setq b9
      (secd-cycle
       nil
       '((a . 12) (b . 5) (R1 . (LDC 2 LDC 8 ADD UPD)) (H . (LDP R1 AP0 LDC 6 ADD UPD)) )
       '(
	 LD b
	 LDP H
	 AP0
	 ADD
	 LDP H
	 AP0
	 ADD
	 STOP
	 )
       nil
       )
      )

;; TODO Revisit promises in letrec blocks
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 DUM
	 LDF ((n) . (LD natnum AP0 RTN)) 
	 LDF (() . (LDC 2 RTN))
	 LDE (LD n AP LDC 1 ADD LD foo AP LD n AP CONS UPD)
	 LDF ((natnum n foo) . (LD natnum AP0 RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )



;;-----------------------------------------------------------------------------
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 LDE (LD a LDC 12 EQ UPD)
	     LD a
	     LDC 5
	     EQ
	     LDE (LDC 15 LDC 20 LEQ UPD)
	     ANY 3
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5))
       '(
	 LDE (LD a LDC 11 EQ UPD)
	     LD a
	     LDC 5
	     EQ
	     LDE (LDC 15 LDC 2 LEQ UPD)
	     ANY 3
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (R1 . (LD a LDC 12 EQ UPD))
	 (R2 . (LDC 15 LDC 2 LEQ UPD))
	 )
       '(
	 LDP R1 
	 LD b
	 LDC 5
	 EQ
	 LDP R2
	 ALL 3
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (C11 . (LD a LDC 12 EQ UPD))
	 (C12 . (LDC 15 LDC 20 LEQ UPD))
	 (C21 . (LDC 15 LDC 2 LEQ UPD))
	 (R1  . (LDP C11 LDP C12 ALL 2 UPD))
	 (R2  . (LDP C21 ALL 1 UPD))
	 (H   . (LDP R1 LDP R2 ANY 2 UPD))
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (R1 . (LD a LDC 12 EQ UPD))
	 (R2 . (LDC 15 LDC 2 LEQ UPD))
	 )
       '(
	 LDP R1 
	 NOT
	 STOP
	 )
       nil
       )
      )

;; With `H1' subgoal (negated)
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (C11 . (LD a LDC 12 EQ UPD))
	 (C12 . (LDC 15 LDC 20 LEQ UPD))
	 (C21 . (LDP H1 NOT UPD))
	 (R1  . (LDP C11 LDP C12 ALL 2 UPD))
	 (R2  . (LDP C21 ALL 1 UPD))
	 (H   . (LDP R1 LDP R2 ANY 2 UPD))
	 (H1  . (LDP R3 ANY 1 UPD))
	 (R3  . (LDP C31 ALL 1 UPD))
	 (C31 . (LD b LDC 12 EQ UPD))
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )

;; Other atom types
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 )
       '(
	 LDE (LDC "Hello" UPD)
	 LDE (LDC "World" UPD)
	 CONS
	 CAR
	 AP0
	 STOP
	 )
       nil
       )
      )
;; recursive lazy evaluation
;; terms(n) := cons( n, terms( n+1 ) )
;; car( cdr( cdr ( terms(1) ) ) )
;; which should return 3
(setq b8
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 )
       '(
	 DUM
	 LDF ((x) . (
		     LDE (
			  LD x
			  LDC 1
			  ADD
			  LDF ((n) . (LD n LD terms AP RTN))
			  AP
			  UPD
			  )
		     LD x
		     CONS
		     RTN
		     ))
	 LDF ((terms) . (LDC 1 LD terms AP CDR AP0 CDR AP0 CDR AP0 CAR RTN))
	 RAP    
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (C11 . (ASK C11 UPD))
	 (C12 . (LDC 15 LDC 15 EQ UPD))
	 (R1  . (LDP C11 LDP C12 ALL 2 UPD))
	 (H   . (LDP R1 ANY 1 UPD))
	 )
       '(
	 LDC 1
	 ASK foo
	 LDC 3    
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (C11 . (ASK C11 UPD))
	 (C12 . (LDC 15 LDC 15 EQ UPD))
	 (R1  . (LDP C11 LDP C12 ALL 2 UPD))
	 (H   . (LDP R1 ANY 1 UPD))
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (C21 . (ASK C21 UPD))
	 (C22 . *F*)
	 (R2  . (LDP C21 LDP C22 ALL 2
		     SEL
		     (LDC *F* JOIN)
		     (LDP C21 AP0
			  SEL (LDC *T* JOIN)
			  (LDP C22 AP0 JOIN)
			  JOIN)
		     UPD))
	 (C11 . (ASK C11 UPD))
	 (C12 . (LDC 15 ASK foo EQ UPD))
	 ;; (R1  . (LDP C11 LDP C12 ALL 2 UPD))
	 (R3  . *F*)
	 (R1  . (LDP C11
		     AP0
		     SEL
		     (LDP C12 AP0 JOIN)
		     (LDC *F* JOIN) UPD)
	      )
	 (H   . (LDP R1 LDP R3 LDP R2 ANY 3
		 SEL
		 (LDC *T* JOIN)    
		 (LDP R1 AP0 SEL (LDC *T* JOIN) (LDP R2 AP0 JOIN) JOIN)
		 UPD)
	      )
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )
(setq b5
      (secd-answer b5 '*F* t))


;; New CPS implementation (in branch)
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (CRT_and_KDU . (ASK CRT_and_KDU LDC "agree" EQ UPD))
	 (C11 . (LDP CRT_and_KDU AP0 UPD))
	 (C12 . (ASK C12 UPD))
	 (R1  . (LDP C12 LDP C11 ALL 2 UPD))
	 (R2  . (LDC 5 LDC 6 EQ UPD))
	 (R3  . *F*)
	 (H   . (LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?

(setq b5
      (secd-answer b5 "agree" t))
What is the value of C12?

(defun comp (e n c)
  (if (atom e)
      (cons 'LDC (cons e c))
    (if (eq (car e) 'car)
	(comp (car (cdr e)) n (cons 'CAR c))
      (if (eq (car e) 'cdr)
	  (comp (car (cdr e)) n (cons 'CDR c))
	(if (eq (car e) 'cons)
	    (comp (car (cdr (cdr e))) n (comp (car (cdr e)) n (cons 'CONS c)))
	  c
	  )
	)
      )
    )
  )

(let ((clist
       (comp '(cons (car (cons 1 2)) (cdr (cons 3 4)))
	     nil '(STOP)))
      )
  (secd-cycle nil nil clist nil)
  )

(comp '(cons (car (cons (quote 1) B)) (cdr (cons C D)))
 nil
 '(STOP)
 )

(setq clist
      (comp '(if A (add '1 '2) (mul '1 '3))
	    nil
	    '(STOP)
	    )
      )
(secd-cycle 'nil '((A . *F*)) clist nil)

(setq clist
      (comp '(lambda (x y) (add x y))
	    nil
	    '(STOP)
	    )
      )
(secd-cycle 'nil '((A . *F*)) clist nil)

(setq clist
      (comp '(let ((fac (lambda (n) (add n '1)))
		   (fbc (lambda (n) (add n '1)))
		   )
	       (fac (quote 2))
	       )
	    nil '(STOP)
	    ))
(secd-cycle 'nil '((A . *F*)) clist nil)

(secd-comp--list '(fac (quote 2)) nil '(STOP))	--- comp--list
	e: (fac '2)
	n: nil
	c: (STOP)


(setq b8
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 )
       '(
	 DUM
	 LDF ((n) LD n LDC 1 ADD RTN)
	 LDF ((fac) . (LDC 2 LD fac AP RTN))
	 RAP    
	 STOP
	 )
       nil
       )
      )
clist

(secd-compile "ex1.lsp")

(setq clist
      (comp '(let ((fac (lambda (n) (if (eq n '1) '1 (mul n (fac (sub '1 n))))))

		   )
	       (fac (quote 5))
	       )
	    nil '(STOP)
	    ))
clist

;; New CPS implementation (in branch)
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 ;; (CRT_and_KDU . (ASK CRT_and_KDU LDC "agree" EQ UPD))
	 ;; (C11 . (LDP CRT_and_KDU AP0 UPD))
	 ;; (C12 . (ASK C12 UPD))
	 ;; (R1  . (LDP C12 LDP C11 ALL 2 UPD))
	 ;; (R2  . (LDC 5 LDC 6 EQ UPD))
	 ;; (R3  . *F*)
	 ;; (H   . (LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 DUM
	 LDE (LDC 5 LDC 6 EQ UPD)
	 LDE (ASK R3 UPD)
	 LDE (LD R1 LD R3 ALL 2 UPD)
	 LDF ((H R3 R1) . (LD H AP0 RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )
What is the value of R3?
What is the value of R3?
What is the value of R3?
(setq b5
      (secd-answer b5 '*T* t))


(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 )
       '(
	 DUM
	 LDF ((n) .  (LDC 1 LD n EQ SEL
			  (LDC 1 JOIN)
			  (LD n LDC 1 SUB LD fac AP LD n MUL JOIN)
			  RTN))
	 LDF ((fac) . (LDC 7 LD fac AP RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )

;; New CPS implementation (in branch)
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 ;; (CRT_and_KDU . (ASK CRT_and_KDU LDC "agree" EQ UPD))
	 ;; (C11 . (LDP CRT_and_KDU AP0 UPD))
	 ;; (C12 . (ASK C12 UPD))
	 ;; (R1  . (LDP C12 LDP C11 ALL 2 UPD))
	 ;; (R2  . (LDC 5 LDC 6 EQ UPD))
	 ;; (R3  . *F*)
	 ;; (H   . (LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 DUM
	 LDE (LDC 5 LDC 5 EQ UPD)
	 LDE (ASK R3 UPD)
	 LDE (LD R1 LD R3 ALL 2 UPD)
	 LDF ((H R3 R1) . (LD H AP0 RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )
What is the value of R3?

(setq b5
      (secd-answer b5 '*T* t))

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 ;; (CRT_and_KDU . (ASK CRT_and_KDU LDC "agree" EQ UPD))
	 ;; (C11 . (LDP CRT_and_KDU AP0 UPD))
	 ;; (C12 . (ASK C12 UPD))
	 ;; (R1  . (LDP C12 LDP C11 ALL 2 UPD))
	 ;; (R2  . (LDC 5 LDC 6 EQ UPD))
	 ;; (R3  . *F*)
	 ;; (H   . (LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 DUM
	 LDE (ASK CRT_and_KDU LDC "agree" EQ UPD)
	 LDE (LD CRT_and_KDU AP0 UPD)
	 LDE (ASK C12 UPD)
	 LDE (LD C12 LD C11 ALL 2 UPD)
	 LDE (LD C12 LD C11 ALL 2 UPD)
	 LDE (LDC *F* UPD)
	 LDE (LD R1 LD R3 LD R2 ANY 3 UPD)
	 LDF ((H R3 R2 R1 C12 C11 CRT_and_KDU) . ( LD H AP0 RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )
What is the value of CRT_and_KDU?
(setq b5
      (secd-answer b5 "disagree" t))
What is the value of C12?
(setq b5
      (secd-answer b5 '*T* t))
(assoc 'C11 (car (cdr b5)))

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 ;; (CRT_and_KDU . (ASK CRT_and_KDU LDC "agree" EQ UPD))
	 ;; (C11 . (LDP CRT_and_KDU AP0 UPD))
	 ;; (C12 . (ASK C12 UPD))
	 ;; (R1  . (LDP C12 LDP C11 ALL 2 UPD))
	 ;; (R2  . (LDC 5 LDC 6 EQ UPD))
	 ;; (R3  . *F*)
	 ;; (H   . (LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 DUM
	 LDE (LDC 3 LDC 5 MUL UPD)
	 LDE (LDC 1 LD funA AP0 ADD UPD)
	 LDF ((funB funA) . (LD funB AP0 RTN))
	 RAP
	 STOP
	 )
       nil
       )
      )

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (CRT_and_KDU . (ASK CRT_and_KDU LDC "agree" EQ UPD))
	 (C11 . (LDP CRT_and_KDU AP0 UPD))
	 (C12 . (ASK C12 UPD))
	 (R1  . (LDP C12 LDP C11 ALL 2 UPD))
	 (R2  . (LDC 5 LDC 6 EQ UPD))
	 (R3  . *F*)
	 (H   . (LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )

(setq kb
      '(
	(rule H ((eq CRT_and_KDU (quote "agree")) (leq PRESSURE (quote 100))))
	(rule H ((eq CRT_and_KDU (quote "disagree")) (leq PRESSURE (quote 50))))
	(rule H1 ((eq DOOR (quote "open"))))
	)
      )

(setq b7 (secd-comp--kb2env kb))

(setq b5
      (secd-cycle
       nil
       b7
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?
What is the value of CRT_and_KDU?

(setq b5
      (secd-answer b5 "disagree" t))
CRT_and_KDU: disagree
nil: *T*
What is the value of DOOR?
What is the value of DOOR?
What is the value of DOOR?
What is the value of DOOR?
What is the value of DOOR?

(setq b5
      (secd-answer b5 "open" t))
DOOR: open
nil: *T*
nil: *T*
nil: *T*
nil: *T*
What is the value of PRESSURE?
What is the value of PRESSURE?
What is the value of PRESSURE?
What is the value of PRESSURE?
What is the value of PRESSURE?
What is the value of PRESSURE?
What is the value of PRESSURE?
(setq b5
      (secd-answer b5 25 t))

(require 'secd-comp-kb)
(setq kb
      '(
	(rule H ((eq CRT_and_KDU (quote "agree"))
		 (leq PRESSURE (quote 100))))
	(rule H ((eq CRT_and_KDU (quote "disagree"))
		 (eq H1 '*T*)
		 (leq PRESSURE (quote 50))))
	(rule H1 ((eq DOOR (quote "open"))))
	)
      )

(setq b7 (secd-comp--kb2env kb))

(secd-compile-sexp--lazy '(x) nil)
(insert (format "%s\n" b7))
((*FWRD-RULES* (R120 H1) (R116 H) (R113 H)) (*FWRD-SIGNS* (DOOR R120) (H1 R116) (PRESSURE R116 R113) (CRT_and_KDU R116 R113)) (H LDP R113 LDP R116 ANY 2 UPD) (H1 LDP R120 ANY 1 UPD) (CRT_and_KDU ASK CRT_and_KDU UPD) (PRESSURE ASK PRESSURE UPD) (DOOR ASK DOOR UPD) (R113 LDP C115 LDP C114 ALL 2 UPD) (C115 LDC 100 LDP PRESSURE AP0 LEQ UPD) (C114 LDC agree LDP CRT_and_KDU AP0 EQ UPD) (R116 LDP C119 LDP C118 LDP C117 ALL 3 UPD) (C119 LDC 50 LDP PRESSURE AP0 LEQ UPD) (C118 LDC *T* LDP H1 AP0 EQ UPD) (C117 LDC disagree LDP CRT_and_KDU AP0 EQ UPD) (R120 LDP C121 ALL 1 UPD) (C121 LDC open LDP DOOR AP0 EQ UPD))

(assoc 'DOOR (cdr (assoc '*FWRD-SIGNS* b7)))
