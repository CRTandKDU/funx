;;; This group of functions is better expressed in continuation
;;; passing style.  They are however equivalent to expansion with
;;; nested SEL/JOIN calls. All are applications (AP0), on boolean
;;; promises, on top of the stack. All are pre-chcking their args.

(defun secd--cps-top (x seq)
  ;; x is an atom, seq a control-list
  (cons x seq))

(defun secd--cps-bot (x seq)
  ;; x is an atom, seq a STOP-terminated control-list
  (append (butlast seq) (list x) (last seq))
  )

(defun secd--cps-set-bot (x seq)
  (let ((bot (copy-tree (last seq))))
    (setcar (last seq) x)
    (setcdr (last seq) bot)
    seq)
  )

(defun secd-seq-cps (s e c d)
  "SEQ <n> Eager sequence.
(<n> . s) e (SEQ <n> . c ) d --> (sequence-of-n . s) e c d

Prerequisite: <n> boolean promises, executed or not, on the
stack. Ignore return.
"
  (list
   s
   e
   (secd--cps-top 'AP0 (secd--cps-top 'CPS (cdr (cdr c))))
   (cons (cons 'SEQ (car (cdr c))) d)
   )
  )

(defun secd-any-cps (s e c d)
  "ANY <n> Eager logical OR.
(<n> . s) e (ANY <n> . c ) d --> (ORed-n . s) e c d

Prerequisite: <n> boolean promises, executed or not, on the
stack. Return *T* only if one *T*-executed value among them.
"
  (let ((prefetch nil)
	(promises nil)
	(n (car (cdr c)))
	)
    (if
	(dotimes (i n prefetch)
	  (setq promises (cons (nth i s) promises))
	  (if (eq (nth i s) secd--ops-true) (setq prefetch t))
	  ;; (insert (format "ANY PRE %d %s\t%s\t%s\n" i (nth i s) prefetch promises))
	  )
	;; An operand is *T*, evals to *T*
	(list (cons secd--ops-true (nthcdr n s)) e (cdr (cdr c)) d)
      (list
       s
       e
       (secd--cps-top 'AP0 (secd--cps-top 'CPS (cdr (cdr c))))
       (cons (cons 'ANY n) d)
       )
      )
    )
  )

(defun secd-all-cps (s e c d)
  "ALL <n> Eager logical AND.
(<n> . s) e (ALL <n> . c ) d --> (ANDed-n . s) e c d

Prerequisite: <n> boolean promises, executed or not, on the
stack. Forces evaluation only if no *F* value among them.
"
  (let ((prefetch nil)
	(promises nil)
	(n (car (cdr c)))
	)
    (if
	(dotimes (i n prefetch)
	  (setq promises (cons (nth i s) promises))
	  (if (eq (nth i s) secd--ops-false) (setq prefetch t))
	  ;; (insert (format "ALL PRE %d %s\t%s\t%s\n" i (nth i s) prefetch promises))
	  )
	(list (cons secd--ops-false (nthcdr n s)) e (cdr (cdr c)) d)
      (list
       s
       e
       (secd--cps-top 'AP0 (secd--cps-top 'CPS (cdr (cdr c))))
       (cons (cons 'ALL n) d)
       )
      )
    )
  )

(defun secd-cps (s e c d)
  "CPS Adjust continuation according to car of d.
(<n> . s) e (CPS .c) (( FLAG . <n> ) . d) -->
either: (*T* | *F* . s) e c d
or: (<n-1> . s) e (AP0 CPS .c) (( WHICH-CPS . <n-1> ) . d)

Prerequisite: <n> boolean promises, executed or not, on the
stack. Forces evaluation according to logic in car d.
"
  (let* ((cps-type (pop d))
	 (n (cdr cps-type))
	 )
    ;; (insert (format "CPS: %s\t%d\n" cps-type n))
    (cond
     (;; Continuation for ALL control
      (eq 'ALL (car cps-type))
      (cond
       ;; Fail to False at first False in ALL
       ((eq secd--ops-false (car s))
	(list (cons secd--ops-false (nthcdr n s)) e (cdr c) d))
       ;; Still True, continue evaluation or exit with True if last promise
       (t
	(if (eq 1 n)
	    ;; Last promise, exit with True
	    (list (cons secd--ops-true (nthcdr n s))
		  e
		  (cdr c)
		  d)
	  ;; More promises to evaluate, apply next one
	  (push (cons (car cps-type) (1- n)) d)
	  (list (cdr s)
		e
		(secd--cps-top 'AP0 (secd--cps-top 'CPS (cdr c)))
		d
		)
	  ))
       )
      )

     (;; Continuation for ANY control
      (eq 'ANY (car cps-type))
      (cond
       ;; Fail to True at first True in ANY
       ((eq secd--ops-true (car s))
	(list (cons secd--ops-true (nthcdr n s)) e (cdr c) d))
       ;; Still False, continue evaluation or exit with False if last promise
       (t
	(if (eq 1 n)
	    ;; Last promise, exit with False
	    (list (cons secd--ops-false (nthcdr n s))
		  e
		  (cdr c)
		  d)
	  ;; More promises to evaluate, apply next one
	  (push (cons (car cps-type) (1- n)) d)
	  (list (cdr s)
		e
		(secd--cps-top 'AP0 (secd--cps-top 'CPS (cdr c)))
		d
		)
	  ))
       )
      )

     (;; Continuation for SEQ control
      (eq 'SEQ (car cps-type))
      (if secd-exec-verbose      
	  (save-current-buffer
	    (set-buffer (get-buffer-create "*SECD*"))
	    (insert (format "CPS SEQ\ns: %s\nc:%s\nd:%s %d\n" s c (car d) n))
	    )
	)
      (if (eq 1 n)
	  ;; Last promise, exit and ignore return
	  (list (nthcdr n s) e (cdr c) d)
	;; More promises
	(push (cons (car cps-type) (1- n)) d)
	(list (cdr s) e (secd--cps-top 'AP0 (secd--cps-top 'CPS (cdr c))) d)
	)
      )

     (t ;; Default: unknown continuation code
      (list s e c d)
      )
     )
    )
  )

(defun secd-not (s e c d)
  "Simply negates top of stack."
  (list
     (cons
      (if (eq secd--ops-false (car s)) secd--ops-true secd--ops-false)
      (cdr s)
      )
     e
     (cdr c)
     d
     )
  )

(provide 'secd-cps-group)

