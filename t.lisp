(define ((succ Int) (x Int))
  (+ x 1))

(succ 1)

(succ "a")

(define ((succ2 Any) (x Int))
  (succ (succ x)))

(succ (succ 2))

(succ2 2)

