;;; Algebraic implementation of context links
;;; Signs-intermediated link between hypos are represented as matrices (Emacs Lisp vectors).
(defun mat-mul (A B)
  ;; A and B vectors (square n x n matrices)
  (let* ((n (length A))
	 (prod (make-vector n 0)))
    (dotimes (i n prod)
      (let ((row (make-vector n 0)))
	(dotimes (j n)
	  (let ((sigma 0))
	    (dotimes (k n sigma)
	      (setq sigma (+ sigma (* (aref (aref A i) k) (aref (aref B k) j)))))
	    (aset row j sigma)))
	(aset prod i row)))))

(defun mat-add (A B)
  (let* ((n (length A))
	 (sum (make-vector n 0)))
    (dotimes (i n sum)
      (let ((row (make-vector n 0)))
	(aset sum i (dotimes (k n row) (aset row k (+ (aref (aref A i) k) (aref (aref B i) k)))))))))

(defun mat-id (n)
  (let ((mat (make-vector n 0)))
    (dotimes (i n mat)
      (let ((row (make-vector n 0)))
	(aset mat i (dotimes (j n row) (aset row j (if (equal i j) 1 0))))))))

(defun mat-nclose (A &optional iterations)
  "Walk matrix of the adjaceny graph of hypotheses."
  (let* ((n (or iterations (1- (length A))))
	 (A-cur A)
	 (Id (mat-id (length A))))
    (dotimes (i n A-cur) (setq A-cur (mat-mul A (mat-add Id A-cur))))))

(defun mat-adjacency (alist)
  "Builds the adjacency of the sign-intermediated graph of hypos."
  (let* ((n (length alist))
	 (adj (make-vector n 0)))
    (dotimes (i n adj)
      (let ((pairi (nth i alist))
	    (row (make-vector n 0)))
	(dotimes (j n row)
	  (let ((pairj (nth j alist)))
	    (aset row j
		  (if (equal i j) 0
		    (if (-intersection (cdr pairi) (cdr pairj)) 1 0)))))
	(aset adj i row)))))

(defun secd-comp-kb-context (alist &optional iterations)
  (let ((env nil)
	(mat (mat-nclose (mat-adjacency alist) iterations)))
    (dotimes (i (length alist) env)
      (setq env (append env (cons (cons (car (nth i alist)) (aref mat i)) nil))))))


;;; Utilities for secd-comp-kb-env
(defun secd-comp-kb-context--nclose (env)
  (let ((alist (cdr (assoc secd--kb-context-signs env))))
    (if alist (secd-comp-kb-context alist) nil)))

(defun secd-comp-kb-context--get (rule)
  (plist-get (if (evenp (length rule)) rule (cdr rule)) :context))

(defun secd-comp-kb-context--merge (lst env)
  ;; (insert (format "CTXT IN> %s\n%s\n" lst env))
  (let ((edges (cdr lst))
	(g-edges (cdr (assoc secd--kb-context-signs env))))
    (if g-edges
	(dolist (edge edges env)
	  (let ((h-edges (cdr (assoc (car edge) g-edges))))
	    ;; (insert (format "\tedge %s\n\thypo %s\n" edge h-edges))
	    (cond
	     (h-edges
	      (dolist (sign (cdr edge))
		(unless (member sign h-edges) (push sign h-edges)))
	      (setcdr (assoc (car edge) g-edges) h-edges))
	     (t (push edge (cdr (assoc secd--kb-context-signs env))))
	     )))
      (push lst env))
    ;; (insert (format "CTXT OUT> %s\n%s\n\n" lst env))
    env))
      

(provide 'secd-comp-kb-context)


