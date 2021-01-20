;;; -*- mode: emacs-lisp; -*-

;; Recursive Lazy Evaluation
;;   terms(n) := cons( n, terms( n+1 ) )
;;   car( cdr( cdr ( terms(1) ) ) )
;; which should return 3
;; ((4) ((a . 12) (b . 5)) (STOP) nil)
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


;; Factorial (recursive)
;; ((5040) ((a . 12) (b . 5)) (STOP) nil)
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

;; Promises in letrec blocks
;; ((16) ((a . 12) (b . 5)) (STOP) nil)
;; Note: environment with redeemed promises is lost in the last RTN
;; check *SECD* buffer for trace.
;; ((16) ((a . 12) (b . 5)) (STOP) nil)
;; In *SECD*:
;; 	UPD
;; s:(16)
;; e:((funB . 16) (funA . 15))
;; c:(RTN)
;; d:(nil ((a . 12) (b . 5)) (STOP))
;; 	RTN
;; s:(16)
;; e:((a . 12) (b . 5))
;; c:(STOP)
;; d:nil
;; 	STOP

(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
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

;; Promises in environment
(setq b5
      (secd-cycle
       nil
       '((a . 12) (b . 5)
	 (CRT_and_KDU . (PROMISE ASK CRT_and_KDU LDC "agree" EQ UPD))
	 (C11 . (PROMISE LDP CRT_and_KDU AP0 UPD))
	 (C12 . (PROMISE ASK C12 UPD))
	 (R1  . (PROMISE LDP C12 LDP C11 ALL 2 UPD))
	 (R2  . (PROMISE LDC 5 LDC 6 EQ UPD))
	 (R3  . *F*)
	 (H   . (PROMISE LDP R1 LDP R3 LDP R2 ANY 3 UPD))
	 )
       '(
	 LDP H
	 AP0    
	 STOP
	 )
       nil
       )
      )

(setq secd-exec-verbose nil)

What is the value of CRT_and_KDU?
(setq b5
      (secd-answer b5 "agree" t))
What is the value of C12?
(setq b5
      (secd-answer b5 '*T* t))
;; ((*T*) ((a . 12) (b . 5) (CRT_and_KDU . "agree") (C11 . *T*) (C12 . *T*) (R1 . *T*) (R2 . *F*) (R3 . *F*) (H . *T*)) (STOP) nil)

What is the value of CRT_and_KDU?
(setq b5
      (secd-answer b5 "disagree" t))
;; ((*F*) ((a . 12) (b . 5) (CRT_and_KDU . "disagree") (C11 . *F*) (C12 ASK C12 UPD) (R1 . *F*) (R2 . *F*) (R3 . *F*) (H . *F*)) (STOP) nil)

;; Switching R3 to *T*
;; No questions asked.
;; ((*T*) ((a . 12) (b . 5) (CRT_and_KDU ASK CRT_and_KDU LDC "agree" EQ UPD) (C11 LDP CRT_and_KDU AP0 UPD) (C12 ASK C12 UPD) (R1 LDP C12 LDP C11 ALL 2 UPD) (R2 LDC 5 LDC 6 EQ UPD) (R3 . *T*) (H . *T*)) (STOP) nil)


