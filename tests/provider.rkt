#lang planet cs019/cs019

(provide (all-defined-out))

(define-struct g (a b))
(define ht (hash))
(define ht2 (hash-set ht "x" 10))

(define-struct: h ([p : Number$] [q : String$]))
