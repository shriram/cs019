#lang s-exp "../cs019.rkt"

(define-struct f (x))
(define-struct g (a b))
(check-expect (build-list 5 add1) (list 1 2 3 4 5))
(check-expect (make-g 1 2) (make-g 1 2))
(check-expect (make-g 'b empty) (make-g 'b empty))

(define i (open-image-url "http://racket-lang.org/logo.png"))
(check-expect (image-height i) 85)
(check-expect (image-width i) 88)

(define ht (hash))
(define ht2 (hash-set ht "x" 10))
(check-expect(hash-ref ht2 "x") 10)
(check-error (hash-ref ht "x") "hash-ref: no value found for key: \"x\"")
