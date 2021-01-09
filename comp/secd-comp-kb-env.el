(defun secd-comp--rule2env (kb rn)
  "Compiles an environment and a list of terminals for rule `rn' in `kb'.
Returns environment and list of terminals found in conditions."
  (let ((env nil))
    (if (eq (car kb) 'rule)
	(let ((source-list nil)
	      (rclist nil)
	      (cclist nil)
	      (aclist nil)
	      (axlist nil)
	      (rhs-rvars nil)
	      (rvars  nil))
	  ;; Compile LHS conditions
	  ;; Push each condition in environment. Accumulate rule, variables.
	  (dolist (c (car (cdr (cdr kb))))
	    (let ((cn (gensym 'C))
		  (ccompiled (secd-compile-sexp--lazy c '(UPD)))
		  )
	      (push (cons cn (cons c nil)) source-list)
	      (setq rclist (cons 'LDP (cons cn rclist)))
	      (setq cclist (push (cons cn (car ccompiled)) cclist))
	      ;; (setq rvars  (append rvars (cadr ccompiled)))
	      ;; Prepare list of signs -> condition forward decorations
	      ;; with the exception of constant promises `*T*' and `*F*'
	      (dolist (var (cadr ccompiled) rvars)
		;; Introduced tests on  `*T*' and `*F*' to handle
		;; (eq var *T*) instead of (eq var (quote *T*)). Useful?
		(if (and (not (equal var secd--ops-true))
			 (not (equal var secd--ops-false)))
		    (push (cons var (cons cn rn)) rvars)))
	      ;; (insert (format "---\nc: %s\nrvars :%s\ncclist: %s\n" c rvars cclist))
	      )
	    )
	  ;; Compile RHS actions if any
	  (dolist (ax (car (cdr (cdr (cdr kb)))))
	    (let ((axn (gensym 'A))
		  (axcompiled (secd-compile-sexp--lazy ax '(UPD)))
		  )
	      (push (cons axn (cons ax nil)) source-list)
	      (setq axlist (cons 'LDP (cons axn axlist)))
	      (setq aclist (push (cons axn (car axcompiled)) aclist))
	      ;; (setq rvars  (append rvars (cadr axcompiled)))
	      (dolist (var (cadr axcompiled) rvars)
		(push (cons var (cons axn rn)) rvars))
	      (setq rhs-rvars  (append rhs-rvars (cddr axcompiled))) 
	      )
	    )
	  ;; (insert (format "---\npost ax: %s\nrv: %s\n" source-list rvars))
	  ;; Generates: (LDP C_i ALL <n> SEL (LDP AX_i SEQ <m> LDC *T* JOIN) (LDC *F* JOIN)
	  (setq
	   rclist
	   (append
	    rclist
	    (cons
	     'ALL
	     (cons
	      (/ (length rclist) 2)
	      (if axlist
		  (cons
		   'SEL
		   (append
		    (list (append
			   axlist
			   (cons 'SEQ
				 (cons (/ (length axlist) 2)
				       (cons 'LDC (cons '*T* (cons 'JOIN nil))))))
		    (cons 'LDC (cons '*F* (cons 'JOIN nil))))
		    (cons 'UPD nil)))
		(cons 'UPD nil))))))

	  (push (cons rn rclist) env)
	  (if (assoc secd--kb-cond-source env)
	      (dolist (x source-list)
		(push x (cdr (assoc secd--kb-cond-source env))))
	    (push (cons secd--kb-cond-source source-list) env)
	    )
	  (setq env (append env aclist))
	  (setq env (append env cclist))
	  (cons env (cons rvars rhs-rvars))
	  )
      nil
      )
    )
  )

(defun secd-comp--kb2env (kb)
  "Compiles a complete environment for knowledge base `kb'."
  ;; Adds typed graph edges in several passes
  (let (
	(env nil)     ;; Environment incrementally built from rule sexps
	(hypos nil)   ;; Collect hypotheses
	(signs nil)   ;; Collect signs, i.e. non-hypo terminals in conds
	(flist nil)   ;; Forward-chaining signs -> rule alist
	(frlst nil)   ;; Forward-chaining rule -> hypo alist
	(blist nil)   ;; Backward-chaining set-variable -> rule
	)
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (erase-buffer)
      (insert (format "-- NEW COMPILE:\n%s\n" kb))
      )
    ;; Pass #1
    ;; Accumulate hypos and associated rules in `hypos'.
    ;; Compiles rules into `env', collecting terminals in `signs'.
    ;; Forward-chaining liks are collected as an alist `(var . (rules))'
    (dolist (rule kb env)
      (let* (
	     (rn (gensym 'R))
	     (rcompiled (secd-comp--rule2env rule rn))
	     )
	(if (assoc rn frlst) (push (cadr rule) (cdr (assoc rn frlst)))
	  (push (cons rn (cons (cadr rule) nil)) frlst))
	(if (assoc (cadr rule) hypos) (push rn (cdr (assoc (cadr rule) hypos)))
	  (push (cons (cadr rule) (cons rn nil)) hypos))

	;; (setq env (append env (car rcompiled)))
	(dolist (lst (car rcompiled))
	  (if (eq secd--kb-cond-source (car lst))
	      (if (assoc secd--kb-cond-source env)
		  (dolist (edge (cdr lst))
		    (push edge (cdr (assoc secd--kb-cond-source env))))
		(push (cons secd--kb-cond-source (cdr lst)) env))
	    (push lst env)))

	;; Separate lists for bwrd on set-variables and fwrd on signs
	;; Merge (var Ci Rj) from different rules
	(dolist (var-c-r (cadr rcompiled) flist)
	  (if (assoc (car var-c-r) flist)
	      (push (cdr var-c-r) (cdr (assoc (car var-c-r) flist)))
	    (push (cons (car var-c-r) (list (cdr var-c-r))) flist)))
	(dolist (var (cddr rcompiled) blist)
	  (if (assoc var blist) (push rn (cdr (assoc var blist)))
	    (push (cons var (cons rn nil)) blist)))
	(dolist (var (cadr rcompiled) signs) (add-to-list 'signs (car var)))
	(dolist (var (cddr rcompiled) signs) (add-to-list 'signs var))
	)
      )
    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 1:\nH: %s\nE: %s\nT: %s\nF: %s\nB: %s\n" hypos env signs flist blist))
      )
    ;; Pass #2
    ;; Increment environment with terminals (both from LHS and from RHS)
    ;; Create *BWRD-SIGNS*
    (push (cons secd--kb-backward-chaining-signs blist) env )
    ;; Push constant promises for *T* and *F*. See also remark above.
    ;; (push (cons secd--ops-true
    ;; 		(cons 'LDC (cons secd--ops-true (cons 'UPD nil))))
    ;; 	  env)
    ;; (push (cons secd--ops-false
    ;; 		(cons 'LDC (cons secd--ops-false (cons 'UPD nil))))
    ;; 	  env)

    (dolist (sign signs env)
      ;; Do nothing if sign is an hypo, backward chaining is built-in
      (if (null (assoc sign hypos))
	  ;; Do we backward chain on RHS settable signs?
	  (let ((rhs-rules
		 (cdr (assoc sign (cdr (assoc secd--kb-backward-chaining-signs env)))))
		)
	    (if (and secd--kb-option-backward-chaining-rhs rhs-rules)
		(let* ((n (length rhs-rules))
		       ;; (ctrls (list 'ANY n 'SEL '(JOIN) (list 'ASK sign 'JOIN) 'UPD))
		       (ctrls (list 'ANY n
				    'SEL
				    (list 'LDP sign 'JOIN)
				    (list 'ASK sign 'JOIN) 'UPD))
		      )   
		  (save-current-buffer
		    (set-buffer (get-buffer-create "*SECD-COMP*"))
		    (insert (format "---RHS B-C:\nS: %s (%d rules)\n" sign n))
		    )
		  (dolist (r rhs-rules)
		    (push r ctrls)
		    (push 'LDP ctrls)
		    )
		  (push (cons sign ctrls) env)
		  )
	      (push (cons sign (cons 'ASK (cons sign (cons 'UPD nil)))) env))
	    )
	;; No: standard ASK <SIGN> UPD control list
	(push (cons sign (cons 'ASK (cons sign (cons 'UPD nil)))) env)
	)
      )

    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 2:\nH: %s\nE: %s\nT: %s\n" hypos env signs))
      )
    ;; Pass #3
    ;; Increment environment with hypos
    (dolist (h hypos env)
      (let ((rlist nil)
	    (suffix (cons 'ANY (cons (length (cdr h)) (cons 'UPD nil))))
	    )
	(dolist (rule (cdr h) rlist)
	  (setq rlist (cons 'LDP (cons rule rlist)))
	  )
	(push (cons (car h) (append rlist suffix)) env)
	)
      )
    ;; Pass #4
    ;; Increment environment with forward-chaining links
    (push (cons secd--kb-forward-chaining-signs flist) env )
    (push (cons secd--kb-forward-chaining-rules frlst) env )
    ;; Increment environment with prompts
    (push (cons secd--kb-prompts (secd-comp--kb-prompts kb)) env) 

    (save-current-buffer
      (set-buffer (get-buffer-create "*SECD-COMP*"))
      (insert (format "---Pass 3, 4:\nH: %s\nE: %s\nT: %s\n" hypos env signs))
      )
    env
    )
  )

(provide 'secd-comp-kb-env)
