(require 'secd-ld-group)
(require 'secd-io-group)
(require 'secd-ops-group)
(require 'secd-fun-group)
(require 'secd-arith-group)
;;; Extensions
(require 'secd-lazy-group)
(require 'secd-cps-group)

(defconst secd--mnemonics
  '(;; Load Group
    (LD		. secd-ld)
    (LDC	. secd-ldc)
    (LDF	. secd-ldf)    
    ;; I/O Group
    (ASK	. secd-ask)
    ;; Operations (ops) Group
    (CAR	. secd-car)
    (CDR	. secd-cdr)
    (ATOM	. secd-atom)
    (CONS	. secd-cons)    
    (EQ	        . secd-eq)
    (LEQ  	. secd-leq)
    (SEL  	. secd-sel)
    (JOIN  	. secd-join)
    ;; Functions (fun) Group
    (AP  	. secd-ap)    
    (RTN  	. secd-rtn)
    (DUM  	. secd-dum)
    (RAP  	. secd-rap)
    ;; Arithmetic Group
    (ADD  	. secd-add)
    (SUB  	. secd-sub)
    (MUL  	. secd-mul)
    (DIV  	. secd-div)
    (REM  	. secd-rem)
    ;;; Extension: Lazy Evaluation Group
    (LDE	. secd-lde)
    (LDP	. secd-ldp)
    (AP0	. secd-ap0)
    (UPD	. secd-upd)
    ;;; Extension: CPS operators
    (ANY	. secd-any-cps)
    (ALL	. secd-all-cps)
    (CPS	. secd-cps)
    (NOT	. secd-not)
    ))

(defun secd--s (state) (car state))
(defun secd--e (state) (car (cdr state)))
(defun secd--c (state) (car (cdr (cdr state))))
(defun secd--d (state) (car (cdr (cdr (cdr state)))))

(defun secd-cycle (stack env control dump)
  "Steps through the control list `c'"
  (let* ((s stack) (e env) (c control) (d dump)
	 ;; `transition' is used for its side effect on `(s e c d)'
	 ;; by calling the instruction implementation found in the mnemonics
	 (transition
	  (lambda (control)
	     (let ((state (funcall control s e c d)))
	       (setq s (secd--s state)
		     e (secd--e state)
		     c (secd--c state)
		     d (secd--d state)
		     )
	       )
	     (list s e c d)
	     )
	  )
	 )
    ;; Trace
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD*"))
      (erase-buffer)
      (insert (format "-- NEW THREAD:\ns:%s\ne:%s\nc:%s\nd:%s\n\t%s\n" s e c d (car c)))
      )
    ;; Steps through the instructions in control list `control'
    (catch 'STOP
      (while c
	;; Trace
	(save-current-buffer
	  (set-buffer (get-buffer-create "*SECD*"))
	  (insert (format "s:%s\ne:%s\nc:%s\nd:%s\n\t%s\n" s e c d (car c)))
	  ;; (insert
	  ;;  (format "%s,%s,%s,%s,%s\n" (car c) s e c d))
	  )
	(cond
	 ;; Exits from while loop
	 ;; With transition to ask a value
	 ((eq 'ASK (car c))
	  (throw 'STOP
		 (funcall transition (cdr (assoc (car c) secd--mnemonics)))))
	 ;; Without transition, ends loop
	 ((eq 'STOP (car c)) (throw 'STOP (list s e c d)))
	 ;; Execute controls from mnemonics alist
	 ((assoc (car c) secd--mnemonics)
	    (funcall transition (cdr (assoc (car c) secd--mnemonics)))
	    )
	 ;; Early control list termination
	 (t (setq c (cdr c)))
	 )
	)
      )
    )
  )

(provide 'secd-exec)
