(define ((cadr any) (x list))
  (car (cdr x)))

(define ((caddr any) (x list))
  (car (cdr (cdr x))))

(define ((caar any) (x list))
  (car (car x)))

(define ((cadar any) (x list))
  (car (cdr (car x))))

(define ((and bool) (x bool) (y bool))
  (cond (x (cond (y true)))
	(true false)))

(define ((or bool) (x bool) (y bool))
  (cond (x true)
	(y true)
	(true false)))

(define ((not bool) (x bool))
  (cond (x false)
	(true true)))

(define ((atom? bool) (x any))
  (cond ((eq? (type x) 'atom) true)
	(true false)))

(define ((zip list) (x list) (y list))
  (cond ((and (null? x) (null? y)) '())
	((and (not (atom? x)) (not (atom? y)))
	 (cons (mklist (car x) (car y))
	       (zip (cdr x) (cdr y))))))

(define ((fst any) (pair list))
  (car pair))

(define ((snd any) (pair list))
  (cadr pair))

(define ((map list) (f any) (xs list))
  (cond ((null? xs) '())
	(true (cons (f (car xs))
		   (map f (cdr xs))))))

(define ((filter list) (p any) (xs list))
  (cond ((null? xs) '())
	((p (car xs))
	 (cons (car xs)
	       (filter p (cdr xs))))
	(true (filter p (cdr xs)))))

(define ((even? bool) (x int))
  (cond ((eq? (mod x 2) 0) true)
	(true false)))

(define ((foldr any) (f any) (x0 any) (xs list))
  (cond ((null? xs) x0)
	(true (f (car xs)
		(foldr f x0 (cdr xs))))))

(define ((sum.i int) (xs list))
  (foldr +.i 0 xs))

(define ((prod.i int) (xs list))
  (foldr *.i 1 xs))

(define ((all bool) (p any) (xs list))
  (foldr and true (map p xs)))

(define ((any bool) (p any) (xs list))
  (foldr or '() (map p xs)))

(define ((assoc any) (x any) (y list))
  (cond ((eq? (caar y) x) (cadar y))
	(true (assoc x (cdr y)))))

(define ((elem? bool) (x any) (xs list))
  (cond ((null? xs) false)
	((eq? (car xs) x) true)
	(true (elem? x (cdr xs)))))

; (define ((replace list) (pairs list) (xs list))
;   (foldr
;     (lambda (x acc)
;       (cond ((eq? (type x) 'list)
; 	     (cons (replace pairs x) acc))
; 	    (true
; 	     (cond ((elem? x (map fst pairs))
; 		    (cons (assoc x pairs) acc))
; 		   (true (cons x acc))))))
;     '()
;     xs))

(define ((length int) (xs list))
  (cond ((null? xs) 0)
	(true (+ 1 (length (cdr xs))))))


(cadr '(1 2 3))
; 2

(caddr '(1 2 3))
; 3

(caar '((1) 2))
; 1

(cadar '((1 2) 2))
; 2

(and true true)
; #t

(and true false)
; '()

(and false true)
; '()

(and false false)
; '()

(or true true)
; true

(or true false)
; true

(or false true)
; true

(or false false)
; '()


(not true)
; '()

(not false)
; true

(define (pairs list) (zip '(0 1 2 3) '(a b c d)))

pairs

(map fst pairs)

(map snd pairs)

(filter even? '(1 2 3 4 5))

(foldr cons '() '(1 2 3 4))

(sum.i '(1 2 3 4 5 6))

(prod.i '(1 2 3 4 5 6))

(foldr and true (map even? '(1 2 3 4 5)))

(foldr and true (map even? '(0 2 4 6 8)))

(all even? '(1 2 3 4 5))

(all even? '(0 2 4 6 8))

(any even? '(1 2 3 4 5))

(any even? '(1 3 5 7 9))

(define (dict list)
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

(length '())
(length '(1 2 3 4))
(length '(1 2 3 4 5))
