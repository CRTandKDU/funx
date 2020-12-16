;; Is the flagged CAR necessary? The `rplaca' should take care of
;; repeated forcing? Only if there are global promises for signs, conds,
;; rules and hypos in the environment.
;; Move to load group?
(defun secd-lde (s e c d)
  "LDE Load expression, builds a promise.
s e (LDE c' . c) d --> ((PROMISE (c' . e)) . s) e c d

where the presence of the boolean indicates a promise and its
status: delayed or forced. No arguments in the closure (compare to LDF).
"
  (list
   ;;    (cons 'PROMISE (cons (cons (car (cdr c)) e) s))
   (cons (cons 'PROMISE  (cons (car (cdr c)) e)) s)
   e
   (cdr (cdr c))
   d
   )
  )

(defun secd-ldp (s e c d)
  "LDP Load promise from environment.
s e (LDP H . c) d --> ((PROMISE (c' . e)) . s) e c d

Associated to H in the environment is either a control list, used
to build a promise as in LDE, or simply the value resulting from
a previously evaluated promise (with AP0) which is then
simply loaded as with LD.
"
  (let ((cprime (secd-ld--locate (car (cdr c)) e))
	)
    (list
     (if (atom cprime)
	 (cons cprime s)
       ;; (cons 'PROMISE (cons (cons cprime e) s))
       (cons (cons 'PROMISE (cons cprime e)) s) 
       )
     e
     (cdr (cdr c))
     d
     )
    )
  )

;; It differs from the AP instruction in two respects. Firstly, it
;; tests whether the value at the top of the stack is in fact a
;; promise; if not, then the APO instruction has no
;; effect. Secondly, when a closure is applied, the second item on
;; the stack is an argument list which is added to the environment
;; of the closure; when a promise is forced, there is no argument
;; list, so the environment for the execution of the code of the
;; promise is just that contained in the promise itself. Notice also
;; that the promise is not removed from the top of the dumped stack,
;; since it will be needed again, by the UPD instruction at the end
;; of the promise code.
(defun secd-ap0 (s e c d)
  "AP0 Application triggers execution of the code in a promise.
( (PROMISE (c' . e')) . s) e (APO . c) d 
--> nil e' c' ({*T*|*F*} (c' . e') . s) e c . d)
on a promise;
(a . s) e (APO . c) d --> (a.s) e c d 
if a is not a promise.

"
  (if (and (listp (car s)) (eq 'PROMISE (car (car s))))
      ;; Trace
      (let* (
	     ;; Issue of DUM + <LDF>s or DUM + <LDE>s?
	     (eprime-F-or-E (cdr (cdr (car s))))
	     (eprime
	      (if (listp (car (car eprime-F-or-E)))
		  (car eprime-F-or-E)
		eprime-F-or-E))
	     (cprime (car (cdr (car s))))
	     (ignore
	      (save-current-buffer
		(set-buffer (get-buffer-create "*SECD*"))
		(insert (format "PROMISE at s:%s\ne:%s\nc:%s\nd:%s\n\t%s\n" s e c d (car c)))
		(insert (format "eprime F/E : %s\t%s\t%s\n"  eprime-F-or-E (listp (car eprime-F-or-E )) (listp (car (car eprime-F-or-E)))))
		(insert (format "eprime : %s\n"  eprime))
		(insert (format "cprime : %s\n" cprime))
		))
	     )
	(list
	 nil
	 eprime
	 cprime
	 (cons s (cons e (cons (cdr c) d)))
	 )
	)
    (list s e (cdr c) d)
    )
  )

;; In order to prevent repeated evaluation of promises, an instruction is
;; required which overwrites a promise with its value, once this value is
;; calculated. Since this overwriting must happen at the end of the execution
;; of the code of a promise, the operation is combined with the RTN operation,
;; and is implemented by the UPD instruction
;; (defun secd-upd (s e c d)
(defun secd-upd (s e c d)
  "UPD Update, terminates a promise's code.
(v) e' (UPD) (s e c.d) --> rplaca(s,v) e c d

If the promise code is found in the environment associated to a unique atom,
it is updated in-place.
"
  (save-current-buffer
    (set-buffer (get-buffer-create "*SECD*"))
    (insert (format "UPD at key:%s\ne:%s\n"
		    (car (cdr (car (car d))))
		    (car (cdr d))))
    )
    
  
  ;; *SIDE-EFFECT*: if the same control list appears in the environment
  ;; the thunk is also updated!
  ;; Two situations: the alist is an environment with no `PROMISE' keyword
  ;; or the alist is an environment built by RAP with `PROMISE' keywords
  (let ((clist (car (cdr (car (car d)))))
	(env   (car (cdr d)))
	       )
	(if (rassoc clist env)
	    ;; Alter promise in environment
	    (setcdr  (rassoc clist env)  (car s))
	  (if (-first (lambda (binding) (and (listp (cdr binding))
					     (equal (caddr binding) clist)))
		      env)
	      (setcdr (-first
		       (lambda (binding)
			 (and (listp (cdr binding))
			      (equal (caddr binding) clist)))
		       env)
		      (car s)))
      )
    )
  (save-current-buffer
    (set-buffer (get-buffer-create "*SECD*"))
    (insert (format "Out of UPD\ns:%s\ne:%s\nc:%s\nd:%s\n\t%s\n"
		    (cons (car s) (cdr (car d)))
		    (car (cdr d))
		    (car (cdr (cdr d)))
		    (cdr (cdr (cdr d)))
		    (car c)))
    )
  (list (cons (car s) (cdr (car d)))
   (car (cdr d))
   (car (cdr (cdr d)))
   (cdr (cdr (cdr d)))
   )
  )

;;; Special lazy OR to bind rules to an hypothesis
;;; Special lazy AND to bind conditions to a rule
;;; Special lazy NOT
;;; See also: Ken Traub
;;; To be revisited:
;;; As implemented ANY, ALL and NOT are deterministic (synchronized) calls.
;;; If a promise cannot be evaluated for lack of data in a condition (ASK)
;;; it is simply skipped and a random boolean is returned. This is due to
;;; the calls to `secd-cycle' inside loops in these functions.
;;; Proper implementation would require a scheduler à la Traub or maybe
;;; Henderson's like handling of non-determinism (SOR/NONE see book).

;; (defun secd-any (s e c d)
;;   "ANY <n> Eager logical OR.
;; (<n> . s) e (ANY <n> . c ) d --> (ORed-n . s) e c d

;; Prerequisite: <n> boolean promises, executed or not, on the
;; stack. Forces evaluation only if no *T* value among them.
;; "
;;   (let ((prefetch nil)
;; 	(promises nil)
;; 	(n (car (cdr c)))
;; 	)
;;     (if
;; 	(dotimes (i n prefetch)
;; 	  (setq promises (cons (nth i s) promises))
;; 	  (if (eq (nth i s) secd--ops-true) (setq prefetch t))
;; 	  ;; (insert (format "ANY PRE %d %s\t%s\t%s\n" i (nth i s) prefetch promises))
;; 	  )
;; 	;; An operand is *T*, evals to *T*
;; 	(list (cons secd--ops-true (nthcdr n s)) e (cdr (cdr c)) d)
;;       ;; No operand is *T*, some may be *F*.
;;       (secd--any-force promises (nthcdr n s) e (cdr (cdr c)) d)
;;       )
;;     )
;;   )

;; This restricted version of ANY only performs the prechecks 
(defun secd-any (s e c d)
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
      (list (cons secd--ops-false (nthcdr n s)) e (cdr (cdr c)) d)
      )
    )
  )

;; ANY-force works like an application on forced arguments.
(defun secd--any-force (promises s e c d)
  "Eager OR evaluation of promises in stacked order.
Skips atoms on the stack are they are by construction `*F*'.
Returns as soon as a promise evaluates to `*T*'
"
  (let
      ((val   nil)
       (plist promises)
       )
    (while (and plist (null val))
      (let*
	  ((p (pop plist))
	   (state (if (atom p) nil
		    ;; (secd-cycle (list p) nil '(AP0 STOP) nil)))
		    (secd-cycle (list p) e '(AP0 STOP) nil)))
	   )
	(if (eq secd--ops-true (car (car state))) (setq val t))
	;; (insert (format "P = %s\t%s\n%s\n" val p state))
	)
      )
    ;; (insert (format "OUT= %s\n" val))
    (list
     (list (if val secd--ops-true secd--ops-false))
     e c d
     )
    )
  )

;; (defun secd-all (s e c d)
;;   "ALL <n> Eager logical AND.
;; (<n> . s) e (ALL <n> . c ) d --> (ANDed-n . s) e c d

;; Prerequisite: <n> boolean promises, executed or not, on the
;; stack. Forces evaluation only if no *F* value among them.
;; "
;;   (let ((prefetch nil)
;; 	(promises nil)
;; 	(n (car (cdr c)))
;; 	)
;;     (if
;; 	(dotimes (i n prefetch)
;; 	  (setq promises (cons (nth i s) promises))
;; 	  (if (eq (nth i s) secd--ops-false) (setq prefetch t))
;; 	  ;; (insert (format "ALL PRE %d %s\t%s\t%s\n" i (nth i s) prefetch promises))
;; 	  )
;; 	;; An operand is *T*, evals to *T*
;; 	(list (cons secd--ops-false (nthcdr n s)) e (cdr (cdr c)) d)
;;       ;; No operand is *T*, some may be *F*.
;;       (secd--all-force promises (nthcdr n s) e (cdr (cdr c)) d)
;;       )
;;     )
;;   )

(defun secd-all (s e c d)
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
	  (insert (format "ALL PRE %d %s\t%s\t%s\n" i (nth i s) prefetch promises))
	  )
	(list (cons secd--ops-true (nthcdr n s)) e (cdr (cdr c)) d)
      (list (cons secd--ops-false (nthcdr n s)) e (cdr (cdr c)) d)
      )
    )
  )
	
;; ANY-force works like an application on forced arguments.
(defun secd--all-force (promises s e c d)
  "Eager AND evaluation of promises in stacked order.
Skips atoms on the stack are they are by construction `*T*'.
Returns as soon as a promise evaluates to `*F*'
"
  (let
      ((val   t)
       (plist promises)
       )
    (while (and plist val)
      (let*
	  ((p (pop plist))
	   (state (if (atom p) nil
		    ;; (secd-cycle (list p) nil '(AP0 STOP) nil)))
		    (secd-cycle (list p) e '(AP0 STOP) nil)))

	   )
	(if (eq secd--ops-false (car (car state))) (setq val nil))
	;; (insert (format "P = %s\t%s\n%s\n" val p state))
	)
      )
    ;; (insert (format "OUT= %s\n" val))
    (list
     (list (if val secd--ops-true secd--ops-false))
     e c d
     )
    )
  )


(provide 'secd-lazy-group)
