;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; (push "C:/Users/jmc/Documents/code/funx" load-path)
(require 'ox-texinfo)
(normal-top-level-add-subdirs-to-load-path)
(require 'secd-exec)
(require 'secd-comp-kb)
(require 'nxp-ency)
(require 'kb-xml-parse)

;; Use: M-x kb-xml-parse-buffer satfault.xml

(setq secd--kb-option-backward-chaining-rhs t)
(setq session
      (nxp-session
       '((rule H1 ((eq CRT_and_KDU (quote "agree"))
		   (eq H4 '*T*)
		   (leq PRESSURE (quote 100))))
	 (rule H2 ((eq TEMPERATURE (quote 100))
		   (leq PRESSURE DELTA)))
	 (rule H2 ((leq VOLUME (quote 10)))
	       ((set ALPHA (quote "on"))
		))
	 (rule H3 ((eq DELTA_RULE (quote 50)))
	       ((set DELTA (quote 200)))
	       )
	 (rule H4 ((eq BETA_RULE (quote 50)))
	       ((set BETA (quote 200)))
	       )
	 )
       )
      )

(setq session
      (nxp-session
       '((rule H1 ((leq PRESSURE LIMIT)))
	 (rule H2 ((leq VOLUME '100)) ((set LIMIT (quote 100))))
	 (rule H3 ((leq TEMP '100)) ((set LIMIT (quote 200))))
	 )
       )
      )

;; SEQ
(secd-cycle
 nil
 '((a . (LDC *T* UPD)) (b . (LDC *F* UPD))
   (R . (LDP a LDP b ANY 2 UPD))
   )
 '(LDP a LDP R SEQ 2 STOP)
 nil
 )

(secd-cycle
 nil
 '((a . 2) (b . 10)
   )
 '(LDP a LDC 1 LEQ LDC *T* EQ AP STOP)
 nil
 )

(setq session (nxp-session '((rule XDRC_FAILURE_OR_BIAS ((eq ACTION_12 *T*)(not (eq pressure_P2 pressure_P5) ) (not (eq pressure_P1 pressure_P5) ) )))))

(require 'kb-xml-parse)

;; Use: M-x kb-xml-parse-buffer satfault.xml
(setq session (nxp-session SATFAULT))

(setq session
      (nxp-session
       '((rule H1 ((leq a '100)
		   (leq b '100)
		   (leq c '100)
		   )
	       )
	 (rule H2 ((leq '100 a))
	       ((set foo '10))
	       )
	 )
       ))

(setq prompts
      (secd-comp--kb-prompts
       '((rule H1 ((leq a (quote 100))
		   (leq a (quote 20))
		   (eq quux (quote 5))
		   (eq foo (quote BAR))
		   ))
	 (rule H2 ((in quux (quote (a b c d e f)))
		   (in foo (quote (BAR BAZ)))
		   (leq (quote 300) a)))
	 )
       )
      )

(setq prompts
      (secd-comp--kb-prompts SATFAULT))

(setq session
      (nxp-session
       '((rule H1 ((leq a '100))
	       ((set b '20))
	       )
	 (rule H2 ((leq '100 b)))
	 (rule H3 ((leq a '50))
	       ((set c '30))
	       )
	       
	 )
       ))


(setq secd--kb-option-backward-chaining-rhs t)

(insert (format "%s\n"
		(cdr (assoc '*SECD-TOPLEVEL-CLIST* (secd--d (cdr (assoc 'QUESTION session)))))
		)
	)

(setq session (nxp-session SATFAULT))



(insert (format "%s\n" (equal '(STOP) (cadddr (assoc 'QUESTION session)))))
t

;; Testing context links
(setq secd--kb-option-context t)
(setq session
      (nxp-session
       '((rule H1 ((leq (mul '2 a) '100)) ((set b '20))
	       :context (S1 S2)
	       )
	 (rule H2 ((leq '100 b))
	       )
	 (rule H1 ((leq a '50)) ((set c '30))
	       :context (S2 S3)
	       )
	 (rule G1 ((leq x '10) (leq y '10))
	       :context (S4 S2)
	       )
	 (rule G2 ((leq x '5) (leq y '5))
	       :context (S4 S5)
	       )
	 )
       ))

(setq secd--kb-option-context t)
(setq secd--kb-option-context-compare
      (lambda (c1 c2) (equal (car c1) (car c2))))
(setq session
      (nxp-session
       '((rule H1 ((leq (mul '2 a) '100)) ((set b '20))
	       :context ((S1 . 0.5) (S2 . 0.7))
	       )
	 (rule H2 ((leq '100 b))
	       )
	 (rule H1 ((leq a '50)) ((set c '30))
	       :context ((S2 . 0.4) (S3 . 0.2))
	       )
	 (rule G1 ((leq x '10) (leq y '10))
	       :context ((S4 . 0.6) (S2 . 0.7))
	       )
	 (rule G2 ((leq x '5) (leq y '5))
	       :context ((S4 . 3) (S5 . 0.3))
	       )
	 )
       ))

(setq kb '((rule H (((lambda (x z) (leq x (add Y z))) '6 '3)
		    (foo '5)
		    )
		 )
	   )
      )
(setq env (secd-comp--kb2env kb))
(setq session (nxp-session kb))

(setq secd-exec-verbose t)
(secd-cycle
 nil
 '(
   (foo . (PROMISE LDF ((n) . (LDC 1 LD n ADD RTN)) UPD))
   )
 '(LDC 13 LDP foo AP0 AP STOP)
 nil
 )

(secd-comp--comp '(lambda (n) (add n (quote 1))) NAMES nil)


(setq session
      (nxp-session-env 
       (secd-comp-kb--initfill
	'(((rule H (((lambda (x z) (leq x (add Y z))) '6 '3)
		    (foo '5)
		    )
		 )
	   )
	  :with ((foo . (lambda (n) (leq n (quote 7)))))))))

(cons 'PROMISE (append (secd-comp--comp '(lambda (n) (leq n (quote 7))) NAMES nil) '(UPD)))







   

      

