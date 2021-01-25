(define cadr
  (lambda (x) (car (cdr x))))

(define caddr
  (lambda (x) (car (cdr (cdr x)))))

(define caar
  (lambda (x)
    (car (car x))))

(define cadar
  (lambda (x)
    (car (cdr (car x)))))

(define and
  (lambda (x y)
    (cond (x (cond (y '#t)))
          ('#t '()))))

(define or
  (lambda (x y)
    (cond (x '#t)
          (y '#t)
          ('#t '()))))

(define not
  (lambda (x)
    (cond (x '())
          ('#t '#t))))

(define atom?
  (lambda (x)
    (cond ((eq? (type x) 'atom) '#t)
          ('#t '()))))

(define zip
  (lambda (x y)
    (cond ((and (null? x) (null? y)) '())
          ((and (not (atom? x)) (not (atom? y)))
           (cons (list (car x) (car y))
                 (zip (cdr x) (cdr y)))))))

(define fst
  (lambda (pair)
    (car pair)))

(define snd
  (lambda (pair)
    (cadr pair)))

(define map
  (lambda (f xs)
    (cond ((null? xs) '())
          ('#t (cons (f (car xs))
                     (map f (cdr xs)))))))

(define filter
  (lambda (p xs)
    (cond ((null? xs) '())
          ((p (car xs))
           (cons (car xs)
                 (filter p (cdr xs))))
          ('#t (filter p (cdr xs))))))

(define even?
  (lambda (x)
    (cond ((eq? (mod x 2) 0) '#t)
          ('#t '()))))

(define foldr
  (lambda (f x0 xs)
    (cond ((null? xs) x0)
          ('#t (f (car xs)
                  (foldr f x0 (cdr xs)))))))

(define sum.i
  (lambda (xs)
    (foldr +.i 0 xs)))

(define prod.i
  (lambda (xs)
    (foldr *.i 1 xs)))

(define all
  (lambda (p xs)
    (foldr and '#t (map p xs))))

(define any
  (lambda (p xs)
    (foldr or '() (map p xs))))

(define assoc
  (lambda (x y)
    (cond ((eq? (caar y) x) (cadar y))
          ('#t (assoc x (cdr y))))))

(define elem?
  (lambda (x xs)
    (cond ((null? xs) '())
          ((eq? (car xs) x) '#t)
          ('#t (elem? x (cdr xs))))))

(define replace
  (lambda (pairs xs)
    (foldr
      (lambda (x acc)
        (cond ((eq? (type x) 'list)
               (cons (replace pairs x) acc))
              ('#t
               (cond ((elem? x (map fst pairs))
                      (cons (assoc x pairs) acc))
                     ('#t (cons x acc))))))
      '()
      xs)))

(define num-op-template
  '(lambda (x y)
     (cond ((and (eq? (type x) 'double)
                 (eq? (type y) 'double))
            (__D_OP__ x y))
           ((and (eq? (type x) 'int)
                 (eq? (type y) 'int))
            (__I_OP__ x y))
           ('#t '()))))

(define mk-num-op
  (lambda (double-op int-op)
    (replace (zip '(__D_OP__ __I_OP__) (list double-op int-op))
             num-op-template)))

(define +
  (mk-num-op '+.f '+.i))

(define *
  (mk-num-op '*.f '*.i))

(define -
  (mk-num-op '-.f '-.i))

(define /
  (mk-num-op '/.f '/.i))

(define length
  (lambda (xs)
    (cond ((null? xs) 0)
          ('#t (+ 1 (length (cdr xs)))))))


(cadr '(1 2 3))
; 2

(caddr '(1 2 3))
; 3

(caar '((1) 2))
; 1

(cadar '((1 2) 2))
; 2

(and '#t '#t)
; #t

(and '#t '())
; '()

(and '() '#t)
; '()

(and '() '())
; '()

(or '#t '#t)
; '#t

(or '#t '())
; '#t

(or '() '#t)
; '#t

(or '() '())
; '()


(not '#t)
; '()

(not '())
; '#t

(define pairs (zip '(0 1 2 3) '(a b c d)))

pairs

(map fst pairs)

(map snd pairs)

(filter even? '(1 2 3 4 5))

(foldr cons '() '(1 2 3 4))

(sum.i '(1 2 3 4 5 6))

(prod.i '(1 2 3 4 5 6))

(foldr and '#t (map even? '(1 2 3 4 5)))

(foldr and '#t (map even? '(0 2 4 6 8)))

(all even? '(1 2 3 4 5))

(all even? '(0 2 4 6 8))

(any even? '(1 2 3 4 5))

(any even? '(1 3 5 7 9))

(define dict
  '((a "entry for a")
    (b "entry for b")
    (c "entry for c")
    (d "entry for d")))

(assoc 'a dict)
(assoc 'b dict)
(assoc 'c dict)
(assoc 'd dict)
(assoc 'e dict)

(replace '((hot cold)) '(it is hot))

(replace '((small big) (simple complicated)) '(this (is a (very small and (simple)) structure)))

(+ 1 2)

(+ (neg 1) 2)

(+ 1.1 2.2)

(* 1 2)

(* 1.1 2.2)

(- 2 1)

(- 2.127 1.23)

(length '())
(length '(1 2 3 4))
(length '(1 2 3 4 5))
