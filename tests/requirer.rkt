#lang planet cs019/cs019

(require "provider.rkt")

(check-expect (hash-ref ht2 "x") 10)
(check-expect (g-a (make-g 1 2)) 1)
(check-expect (make-h 1 "x") (make-h 1 "x"))
(check-error (make-h "1" "x"))
(check-expect (only-positive-add1 5) 6)
(check-error (only-positive-add1 -1))
(check-expect (match (make-h 3 "x")
                [(struct h (n _)) n])
              3)
(check-expect
 (shared ([H (make-h N S)]
          [N 3]
          [S "X"])
   H)
 (make-h 3 "X"))
