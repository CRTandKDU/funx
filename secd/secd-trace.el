(require 'secd-exec)

(defconst secd-trace--buffer "*SECD-PROTOCOL*")
(defconst secd--mnemonics-width
  '(;; Load Group
    (LD		. 1)
    (LDC	. 1)
    (LDF	. 1)    
    ;; I/O Group
    (ASK	. 0)
    (STOP       . 0)
    ;; Operations (ops) Group
    (CAR	. 0)
    (CDR	. 0)
    (ATOM	. 0)
    (CONS	. 0)    
    (EQ	        . 0)
    (LEQ  	. 0)
    (IN  	. 0)
    (SEL  	. 2)
    (JOIN  	. 0)
    (SET        . 0)
    ;; Functions (fun) Group
    (AP  	. 0)    
    (RTN  	. 0)
    (DUM  	. 0)
    (RAP  	. 0)
    ;; Arithmetic Group
    (ADD  	. 0)
    (SUB  	. 0)
    (MUL  	. 0)
    (DIV  	. 0)
    (REM  	. 0)
    ;;; Extension: Lazy Evaluation Group
    (LDE	. 1)
    (LDP	. 1)
    (AP0	. 0)
    (UPD	. 0)
    ;;; Extension: CPS operators
    (ANY	. 1)
    (ALL	. 1)
    (SEQ	. 1)
    (CPS	. 0)
    (NOT	. 0)
    ))

(setq secd-trace--pc 0)

(defun secd-trace--pp (s)
  (mapcar
   #'(lambda (elt)
       (cond
	((atom elt) elt)
	((eq (car elt) 'PROMISE) (list 'PROMISE (cadr elt) '<ENV>))
	(t elt)))
   s))

(defun secd-trace-init (&optional step)
  (with-current-buffer (get-buffer-create secd-trace--buffer)
    (erase-buffer))
  (setq secd-trace--pc (if step step 0)))

(defun secd-trace-hook (state)
  (let ((s (secd--s state))
	(c (secd--c state)))
    (with-current-buffer (get-buffer-create secd-trace--buffer)
      (setq secd-trace--pc (1+ secd-trace--pc))
      (goto-char (point-max))
      (let ((width (cdr (assoc (car c) secd--mnemonics-width))))
	(insert
	 (format "%4d> %4s\t%s\t%s\t|%s\n"
		 secd-trace--pc
		 (car c)
		 (if (eq 1 width) (cadr c) " ")
		 (if (eq 2 width) (caddr c) " ")
		 (secd-trace--pp s)
		 ))))))

(provide 'secd-trace)
