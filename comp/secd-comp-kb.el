;;; A functional compiler variant:  compiling knowledge bases to environments
(require 'secd-env-group)
(require 'secd-cps-group)
(require 'secd-exec)
(require 'secd-comp)

;; @cindex Decorations
;; Global alists, in the KB scope, stored in the compiled environment for use
;; by the inference engine or the client user interface.

;; @cindex OR-AND Tree
;; KB are compiled to a forest of or-and trees (hypos to rules, rules to
;; conditions). Each node of such trees are compiled to a promise in the
;; environment.

;; Forward chaining decorations
(defconst secd--kb-forward-chaining-signs	'*FWRD-SIGNS*)
(defconst secd--kb-forward-chaining-rules	'*FWRD-RULES*)
(defconst secd--kb-backward-chaining-signs	'*BWRD-SIGNS*)
(defconst secd--kb-toplevel-control-list	'*SECD-TOPLEVEL-CLIST*)

;; CLI decorations
(defconst secd--kb-prompts			'*KB-PROMPTS*)

;; Backward chaining tags specifying the origin of a variable, cond/action.
;; Variables in conditions and actions are also compiled to promises.
(defconst secd--kb-RHS-set-variable	'*RHS*)
(defconst secd--kb-LHS-variable		'*LHS*)
(defconst secd--kb-cond-source		'*COND-SOURCE*)
(defconst secd--kb-context-signs	'*CONTEXT-SIGNS*)
(defconst secd--kb-context-hypos	'*CONTEXT-HYPOS*)


;; WHAT-IF decorations

;; Heuristics options
;; If true, rule values post their hypo for evaluation only if `*T*' (gating on)
(defvar secd--kb-option-forward-chaining-gate t)
;; If true, when signs need evaluation backward immediately on rules the RHS
;; of which `set's the signs. (Note: backward on hypos is automatic.)
(defvar secd--kb-option-backward-chaining-rhs nil)

;; KB compiler components
(require 'secd-comp-kb-sexp)
(require 'secd-comp-kb-prompts)
(require 'secd-comp-kb-context)
(require 'secd-comp-kb-env)

;;; Decompiling control lists to LHS and RHS source
(defun secd-comp-lhs (rule env)
  (let* ((ccode (cdr (assoc rule env)))
	 (ccode-lhs (--take-while (null (eq it 'ALL)) ccode))
	 (ccode-conds (reverse (--remove (eq it 'LDP) ccode-lhs))))
    ccode-conds))

(defun secd-comp-rhs (rule env)
  (let* ((ccode (cdr (assoc rule env)))
	 (ccode-rhs (--drop-while (null (eq it 'SEL)) ccode))
	 (ccode-actions
	  (and ccode-rhs
	       (--remove (eq it 'LDP)
			 (--take-while (null (eq it 'SEQ))
				       (cadr ccode-rhs))))))
    ccode-actions))

;;; High-Level Interface to KB compilers
(defun secd-comp--kb-toplevel-clist (state)
  "Push the top level control list (from `suggest'/`volunteer' commands) into the D register for later use by CPS controls."
  (cdr (assoc secd--kb-toplevel-control-list (secd--d state)))
  )

;;; What-if command
(defun secd-comp--kb-reset (promise session)
  (let ((p-in-env (assoc promise (cdr (assoc 'ENVIRONMENT session))))
	(p-in-fkb (assoc promise (cdr (assoc 'FASKB session)))))
    ;; (with-current-buffer (get-buffer-create "*NXP-SESSION*")
    ;;   (goto-char (point-max))
    ;;   (insert (format "-- WHAT IF p: %s\n%s\n" p-in-env p-in-fkb))
    ;;   )
    (setcdr p-in-env (copy-tree (cdr p-in-fkb))))
  promise
  )

(defun secd-comp--kb-whatif (var val session)
  "Invalidates current forward chaining nodes and resumes session."
  (let ((top-hypos nil)
	(cond-rule-list
	 (cdr
	  (assoc var
		 (cdr
		  (assoc secd--kb-forward-chaining-signs
			 (cdr (assoc 'ENVIRONMENT session))))))))
    (dolist (c-r cond-rule-list)
      (with-current-buffer (get-buffer-create "*NXP-SESSION*")
	(goto-char (point-max))
	(insert (format "- WHAT IF C.R : %s\n" c-r))
	)
      (secd-comp--kb-reset (car c-r) session) ;; Reset condition
      (secd-comp--kb-reset (cdr c-r) session) ;; Reset rule
      ;; From rule reset hypo
      (let ((h (cadr (assoc (cdr c-r)
			   (cdr (assoc secd--kb-forward-chaining-rules
				       (cdr (assoc 'ENVIRONMENT session))))))))
	(with-current-buffer (get-buffer-create "*NXP-SESSION*")
	  (goto-char (point-max))
	  (insert (format "- WHAT IF H: %s\n" h))
	  )
	(secd-comp--kb-reset h session)       ;; Reset hypo
	(add-to-list 'top-hypos h)
	)
      ;; From rule reset RHS
      (let ((actions (secd-comp-rhs (cdr c-r) (cdr (assoc 'FASKB session)))))
	(with-current-buffer (get-buffer-create "*NXP-SESSION*")
	  (goto-char (point-max))
	  (insert (format "- WHAT IF Actions: %s\n" actions))
	  )
	(when actions
	  (dolist (action actions)
	    (secd-comp--kb-reset action session))))
      )
    top-hypos
    )
  )


;;; Forward-chaining hook: signs to rules, conditionally rules to hypos
(defun secd-comp--kb-forward-hook (var val state)
  "General forward-chaining hook, triggered on each environment update."
  ;; Searching for the top-level control list
  ;; (let ((tlcl
  ;; 	 (or
  ;; 	  (and (car (last (secd--d state)))
  ;; 	       (null (eq 'SEQ (car (car (last (secd--d state))))))
  ;; 	       (car (last (secd--d state))))
  ;; 	  (secd--c state))))
  (let ((tlcl (secd-comp--kb-toplevel-clist state)))
    ;; Entry
    (if secd-exec-verbose
	(save-current-buffer
	  (set-buffer (get-buffer-create "*SECD*"))
	  (goto-char (point-max))
	  (let ((cstr (format "#%02X%02X%02X" 0 255 128)))
	    (insert
	     (propertize
	      (format "FWRD-HOOK: On %s (%s):\n %s\nTLCL: %s\n"
		      var val
		      (car (last (secd--d state))) tlcl)
	      'face `(foreground-color . ,cstr))
	     )
	    )
	  )
      )
    ;; Pass #1: A rule truth value forwards its yet unevaluated hypo
    (let ((r-to-h (cdr (assoc secd--kb-forward-chaining-rules (secd--e state))))
	  )
      ;; Is rule in FWRD-CHAIN rule-to-hypo alist?
      (if (assoc var r-to-h)
	  ;; Is forwarding-on-true-rules-only ON?
	  (if (or (null secd--kb-option-forward-chaining-gate)
		  (and  secd--kb-option-forward-chaining-gate
			(equal val secd--ops-true)))
	      ;; Find the top-level control list which is either in c
	      ;; if we already are at top-level or in the last element of d
	      ;; if we are in an application of a promise (through `A0').
	      (let (;;(tlcl  (or (car (last (secd--d state))) (secd--c state)))
		    (hypos (cdr (assoc var r-to-h)))
		    )
			  
		(if secd-exec-verbose
		    (save-current-buffer
		      (set-buffer (get-buffer-create "*SECD*"))
		      (goto-char (point-max))
		      (insert (format "\tFWRD GATE:%s (%s):\n\t %s\n"
				      var val	tlcl))
		      )
		  )
		(dolist (hypo
			 hypos
			 (let ((nhypos (length hypos)))
			   (when (> nhypos 0)
			     (secd--cps-set-bot 'SEQ tlcl)
			     (secd--cps-set-bot nhypos tlcl)
			     )
			   tlcl)
			 )
		  (when (listp (cdr (assoc hypo (secd--e state))))
		    (secd--cps-set-bot 'LDP tlcl)
		    (secd--cps-set-bot hypo tlcl)
		    )
		  )
		)
	    )
	)
      )
    (if secd-exec-verbose
	(save-current-buffer
	  (set-buffer (get-buffer-create "*SECD*"))
	  (goto-char (point-max))
	  (let ((cstr (format "#%02X%02X%02X" 0 255 128)))
	    (insert
	     (propertize
	      (format "FWRD (1) d: %s\n" (secd--d state))
	      'face `(foreground-color . ,cstr))
	     )
	    )
	  )
      )

    ;; Pass #2: A known sign forwards its conditions, then rules
    ;; Find top-level control list, see above.
    (let (;;(tlcl  (or (car (last (secd--d state))) (secd--c state)))
	  (cond-rule-list
	   (cdr
	    (assoc var
		   (cdr
		    (assoc secd--kb-forward-chaining-signs (secd--e state)))))))
      (dolist (c-r
	       cond-rule-list
	       (let ((npromises (* 2 (length cond-rule-list))))
		 (when (> npromises 0)
		   (secd--cps-set-bot 'SEQ tlcl)
		   (secd--cps-set-bot npromises tlcl)
		   )
		 tlcl
		 )
	       )
	(when (listp (cdr (assoc (car c-r) (secd--e state))))
	  (secd--cps-set-bot 'LDP tlcl)
	  (secd--cps-set-bot (cdr c-r) tlcl)
	  (secd--cps-set-bot 'LDP tlcl)
	  (secd--cps-set-bot (car c-r) tlcl)
	  )
	)
      )
    (if secd-exec-verbose
	(save-current-buffer
	  (set-buffer (get-buffer-create "*SECD*"))
	  (let ((cstr (format "#%02X%02X%02X" 0 255 128)))
	    (insert
	     (propertize
	      (format "FWRD (2) d: %s\n" (secd--d state))
	      'face `(foreground-color . ,cstr))
	     )
	    )
	  )
      )
    )
  )

;; To be further refined to work on a list of hypos
(defun secd-comp--kb-knowcess (e goals &optional s var val)
  "A high-level function to start evaluation of hypothesis `goals'."
  (let ((clist (cons 'STOP nil)))
    (dolist (goal goals clist)
      (setq clist (cons 'LDP (cons goal (cons 'AP0 clist))))
      )
    (add-hook 'secd-env-update-hook 'secd-comp--kb-forward-hook)
    (let ((state (list s e clist
		       (cons (cons secd--kb-toplevel-control-list clist) nil))))
      (if (and var val)
	  (secd-env--update e var val state))
      (secd-cycle (secd--s state) (secd--e state) (secd--c state) (secd--d state))
      )
    )
  )

(provide 'secd-comp-kb)
