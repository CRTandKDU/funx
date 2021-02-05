(let
    (
     (secd-comp--vars
      (lambda (elist)
	(if (eq elist nil) nil
	  (cons (car (car elist)) (secd-comp--vars (cdr elist))))))
     )
  (secd-comp--vars '((A 1) (B 2))))
