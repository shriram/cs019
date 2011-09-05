#lang s-exp "../cs019.rkt"

(require "provider.rkt")

(check-expect (hash-ref ht2 "x") 10)
(check-expect (g-a (make-g 1 2)) 1)
(check-expect (make-h 1 "x") (make-h 1 "x"))
(check-error (make-h "1" "x"))
