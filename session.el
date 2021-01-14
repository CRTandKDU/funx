;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; (push "C:/Users/jmc/Documents/code/funx" load-path)
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
























