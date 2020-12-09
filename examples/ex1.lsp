(let
    ((fac
      (lambda (n)
	(if (eq n '1) '1 (mul n (fac (sub '1 n))))))
     )
  (fac (quote 3))
  )
