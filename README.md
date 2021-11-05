# Haskell Lisp

A purely functional, typechecked Lisp written in Haskell.

```lisp
(define ((fib int) (x int))
  (+.i (fib (-.i x 1) (-.i x 2))))

(define ((elem? bool) (x any) (xs list))
  (cond ((null? xs) false)
	((eq? (car xs) x) true)
	(true (elem? x (cdr xs)))))
```

Build and run using [Stack](https://docs.haskellstack.org/en/stable/README/).

```sh
$ stack build
$ stack run script.lisp # run a script
$ stack run -- -r       # repl
```

## TODO List

- Actual closures
- IO capabilities
- Tail-call optimization
- General optimization

## References

- [Roots of Lisp - Paul Graham](http://www.paulgraham.com/rootsoflisp.html)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)
- [Write Yourself a Scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
