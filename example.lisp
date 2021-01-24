"Hello world!"
(cons 0 '(1 2 3))
(car '(1 2 3))
(cdr '(1 2 3))
(list 1 2 3)
(null? '())
(null? '(1 2 3))
(type 1)
(type 1.13)
(type '(1 2 3))
(type 'a)
(type "Hi there")
((lambda (x) (cons x '(1 2 3))) 0)

(let ((x 1)
      (y 2)
      (z 3))
  (cons x (cons y (cons z '()))))


(define x 'a)

x

(eq? 'a 'a)

(eq? 'a 'b)

(eq? x 'a)

(eq? x 'b)

(cond ((eq? x 'a) "x is 'a")
      ((eq? x 'b) "b is 'b")
      ('#t "x is neither 'a nor 'b"))
