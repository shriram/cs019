#lang s-exp "../cs019.rkt"

;; need a test for the passing of source to the wrap in structure setters
;; incorporate commented-out lines from tests/sigs.rkt

;(define-struct: m ([x : 10]
;                   [y : number?]))
;(make-m 1 2)

;(define: (f [l : (Listof: number?)]) -> Number$ (length l))
;(f (list 1 2))

(define BadSig1$ (or: (Number$ -> Number$) Number$))
;(define: bs1 : BadSig1$ 3)
(define BadSig2$ (and: (Number$ -> Number$) Number$))
;(define: bs2 : BadSig2$ 3)
;(define BadSig3$ (not: (Number$ -> Number$)))
;(define: bs3 : BadSig3$ 3)

(define: (f [ l : (Listof: Number$)]) -> Number$
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (first l) (f (rest l)))]))
;(add1 (f 3))
;(add1 (f (list 1 2 "3")))

(define: (n [l : (Listof: (Number$ -> Number$))]) -> (Listof: Number$)
  (map (lambda (f) (f 10)) l))
;(n (list add1 number->string))
;(check-error (n (list add1 string->number)))

(define: (vf [v : (Vectorof: Number$)]) -> Number$
  (vector-ref v 0))
;(vf (vector "a" "b" "c"))
;(vf 3)
;(define: vk : (Vectorof: Number$) (vector 1 2 "3"))

(define: (funf [f : (Number$ -> Number$)]) -> Number$
  (f 10))
;(funf 3)

;((lambda: ([m : Number$] [n : Number$]) -> Number$ +) 1 2)
;(define: v : (not: (Number$ -> Number$)) 5)
(define: s2n : (String$ -> Number$) string->number)
;(s2n "x")
;(define: s2n>> : (String$ ->> Number$) string->number)

(define Tree$ (or: mt$ nd$))
(define-struct mt ())
(define mt$ (Sig: mt?))
(define-struct: nd ([v : Number$] [l : Tree$] [r : Tree$]))
(define: (tree-sum (t : Tree$)) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) (+ (nd-v t) (tree-sum (nd-l t)) (tree-sum (nd-r t)))]))
;(tree-sum (make-nd 10 
;                   (make-nd 5 (make-mt) (make-mt))
;                   (make-nd 2 (make-nd 1 (make-mt) 10) (make-mt))))

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
Number$
Number$
(Sig: (Number$ -> String$))
