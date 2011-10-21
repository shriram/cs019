#lang racket/base

#|
Why on earth are these here?
Because first, etc. don't work on cyclic lists:
(define web-colors
 (shared ([W (cons "white" G)]
          [G (cons "grey" W)])
   W))
(first web-colors)
fails with expected argument of type <list>.
But car/cdr still do the trick per email from mflatt, 10/20/2011.

So we suppress the built-in functions from lang/htdp-advanced
and provide these instead.
|#

(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define (fifth l) (list-ref l 4))
(define (sixth l) (list-ref l 5))
(define (seventh l) (list-ref l 6))
(define (eighth l) (list-ref l 7))

(provide first rest second third fourth fifth sixth seventh eighth)

