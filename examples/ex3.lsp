(let
    ((foo (lambda (x) (add y (mul (quote 2) x))))
     (y (quote 4))
     )
  (foo (quote 3))
  )

