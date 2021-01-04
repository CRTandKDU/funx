;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; (push "C:/Users/jmc/Documents/code/funx" load-path)
(normal-top-level-add-subdirs-to-load-path)
(require 'secd-exec)
(require 'secd-comp-kb)
(require 'nxp-ency)

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
       '((rule H1 ((leq a '100)))
	 (rule H2 ((leq '100 a)))
	 )
       ))


