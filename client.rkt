#lang s-exp "cs019.rkt"

(require (only-in racket/base list->vector))
(require racket/tcp)

true

'(1 2 3)

(list 1 2 3)

(build-list 10 add1)

22/7

(vector 1 2 3)

(define-struct f (x))
(make-f 10)
(make-f (make-f 10))

(define h (make-hash empty))

(define-struct transition (from via to))
(define sm
  (list
   (make-transition "a" "b" "c")
   (make-transition "d" "e" "f")
   (make-transition "g" "h" "i")))
sm

22/7
(build-list 3 add1)
(define-struct g (a b))
(make-g 1 2)
; This is a real problem:
;   Actual value (1 2) differs from (3 4), the expected value.
(check-expect (list 1 2) (list 3 4))

(require 2htdp/image)
(define i (open-image-url "http://racket-lang.org/logo.png"))
(image-height i)
(image-width i)

(define ht (hash))
(define ht2 (hash-set ht "x" 10))
(hash-ref ht2 "x")
(check-error (hash-ref ht "x") "hash-ref: no value found for key: \"x\"")