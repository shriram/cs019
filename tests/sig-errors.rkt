#lang s-exp "../cs019.rkt"

;(define-struct: m ([x : 10]
;                   [y : number?]))
;(make-m 1 2)

;; need a test for the passing of source to the wrap in structure setters

;(define: (f [l : (Listof: number?)]) -> Number$ (length l))
;(f (list 1 2))

;(define: v : (Vectorof: number?) (vector 1 2 3))

;(define: f : (number? number? -> number?) +)
;(f 1 2)

;(define: (f [n : 27]) -> Number$ (add1 n))
;(+ 1 (f 10))

;(define: (f [n : Number$]) -> 27 (add1 n))
;(+ 1 (f 10))

;(define: (f [n : number?]) -> number? (add1 n))
;(+ 1 (f 10))

;((lambda: ([m : number?] [n : number?]) -> number? +) 1 2)

;(define: v : (or: string? 3) 5)
;(define: v : (and: number? 5) 5)
;(define: v : (not: number?) 5)
;(define: v : (not: (Number$ -> Number$)) 5)