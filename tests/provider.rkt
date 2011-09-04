#lang s-exp "../cs019.rkt"

(provide (all-defined-out))

(define-struct g (a b))
(define ht (hash))
(define ht2 (hash-set ht "x" 10))
