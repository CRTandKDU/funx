(let
    ((secd-comp--comp (lambda (e n c) c))
     (secd-comp--list
      (lambda (elist n c)
	(if (eq elist nil) c
	  (secd-comp--list (cdr elist) n
			   (secd-comp--comp (car (cdr (car elist))) n c)
			   ))))
     (secd-comp--vars
      (lambda (elist)
	(if (eq elist nil) nil
	  (cons (car (car elist)) (secd-comp--vars (cdr elist))))))
     )
  (secd-comp--vars '((A 1) (B 2))))
