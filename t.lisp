(define ((succ int) (x int)) (+.i x 1))

(succ 1)

(succ "a")

(define ((succ2 any) (x int)) (succ (succ x)))

(succ (succ 2))

(succ2 2)

