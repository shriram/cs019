#lang s-exp "../cs019.rkt"

(define: x : Number$ 3)
;(define: y : string? 4)
(check-expect x 3)

(define: (g [x : Number$]) -> String$ "x")
(check-expect (g 10) "x")
(check-error (g "x"))

(define: (unchk [x : Any$]) -> Number$ (add1 x))
(check-expect (unchk 10) 11)
(check-error (unchk "x"))

(define: (f [x : Number$] [y : Number$]) -> Number$
  (+ x y))
(check-expect (f 10 12) 22)

(define-struct: swf ([f : (Number$ -> Number$)]))
(check-expect ((swf-f (make-swf add1)) 10) 11)

(define Tree$ (or: mt$ nd$))
(define-struct mt ())
(define mt$ (pred->sig mt?))
(define-struct: nd ([v : Number$] [l : Tree$] [r : Tree$]))

(define: (a (t : mt$)) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) 1]))
(check-expect (a (make-mt)) 0)
(check-error (a (make-nd 1 2 3)))
(check-error (a 3))

(define: (tree-sum (t : Tree$)) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) (+ (nd-v t) (tree-sum (nd-l t)) (tree-sum (nd-r t)))]))
(check-expect (tree-sum (make-nd 10 
                                 (make-nd 5 (make-mt) (make-mt))
                                 (make-nd 2 (make-nd 1 (make-mt) (make-mt)) (make-mt))))
              18)
(check-error (tree-sum (make-nd 10 
                                (make-nd 5 (make-mt) (make-mt))
                                (make-nd 2 (make-nd 1 (make-mt) 10) (make-mt)))))

(define: (prime? [n : (pred->sig (lambda (n) (and (positive? n) (integer? n))))]) 
  -> Boolean$
  (local ([define (check k)
            (if (>= k n)
                true
                (if (= (remainder n k) 0)
                    false
                    (check (add1 k))))])
    (check 2)))
(check-expect (prime? 10) false)
(check-expect (prime? 5) true)
(check-error (prime? -1))
(check-error (prime? 1.5))

(define BadSig$ (or: (Number$ -> Number$) Number$))
;(define: bs : BadSig 3)
;(define BadSig2 (not: (Number$ -> Number$)))

(define VerySpecialNumber$ (and: Number$ (pred->sig positive?) (pred->sig even?)))
(define: vsn : VerySpecialNumber$ 20)
(check-expect vsn 20)
;(define: vsn2 : VerySpecialNumber$ 19)
(define: nvsn : (not: VerySpecialNumber$) 19)
(check-expect nvsn 19)

(define-struct: p ([x : Number$] [y : Number$]))
(define: (h [p : p$]) -> Number$
  (p-x p))
(check-expect (h (make-p 1 2)) 1)
(check-expect (h (make-p 3 4)) 3)

(define n->n (proc: (Number$ -> Number$)))
(define: a1 : n->n add1)
(check-expect (a1 5) 6)
(check-error (a1 "x"))

;(define JunkSig (proc: Number$))

(define: a2 : (Number$ -> Number$) add1)
(check-expect (a2 5) 6)
(check-error (a2 "x"))

(define: s2n : (String$ -> Number$) string->number)
(check-expect (s2n "123") 123)
(check-error (s2n "xyz")) ;; produces false

(define: (i [f : (Number$ -> Number$)]) -> Number$
  (f 5))
(check-expect (i add1) 6)
(check-error (i number->string))
(check-error (i string->number))

(define: (j [f : (String$ String$ String$ -> Number$)]) -> Number$
  (f "12" "34" "56"))
(check-expect (j (lambda (s1 s2 s3)
                   (string->number (string-append s1 s2 s3))))
              123456)
(check-error (j string-append))

(define: (j2 [f : (String$ String$ -> String$)] [g : (String$ -> String$)]) -> String$
  (g (f "abc" "def")))
(check-expect (j2 string-append (lambda (s) (substring s 0 2))) "ab")

(define: (d/dx [f : (Number$ -> Number$)]) -> (Number$ -> Number$)
  (local [(define: dx : Number$ 0.0001)]
    (lambda: ([x : Number$]) -> Number$
      (/ (- (f (+ x dx)) (f x))
         dx))))
(check-within ((d/dx (lambda: ([x : Number$]) -> Number$ (* x x))) 10) 19 21)
(check-error ((d/dx number->string) 10))

(check-within
 ((lambda: ([ddx : ((Number$ -> Number$) -> (Number$ -> Number$))]) -> Number$
           ((ddx (lambda (x) (* x x))) 10))
  d/dx)
 19 21)

(check-expect (local ([define: x : Number$ 3]) x) 3)
(check-error (local ([define: x : String$ 3]) x))

(check-expect (local ([define: (f [x : Number$]) -> String$
                        (number->string x)])
                (f 10))
              "10")
(check-expect (local ([define: (f [p : (Number$ -> String$)]) -> String$
                        (p 10)])
                (f number->string))
              "10")
(check-error (local ([define: (f [x : Number$]) -> String$
                        (number->string x)])
                (f "10")))

(check-expect (local ([define-struct: m ([v : Number$] [w : String$])])
                (m-v (make-m 5 "x")))
              5)
(check-error (local ([define-struct: m ([v : Number$] [w : String$])])
               (m-v (make-m "x" 5))))

(define: l : (Listof: Number$) (list 1 2 3))
(check-expect l (list 1 2 3))
; (define: m : (Listof: Number$) (list 1 2 "X"))

(define: (n [l : (Listof: (Number$ -> Number$))]) -> (Listof: Number$)
  (map (lambda (f) (f 10)) l))
(check-expect (n (list add1 sub1)) (list 11 9))
(check-error (n (list add1 number->string)))
(check-error (n (list add1 string->number)))

(define: vs : (Vectorof: String$)
  (vector "0" "1" "2"))
(check-expect vs (vector "0" "1" "2"))

(define: (cvts [ns : (Listof: Number$)]) -> (Listof: String$)
  (local [(define: cv : (Vectorof: ((Listof: String$) -> (Listof: String$)))
            (vector (lambda (cs) (cons "0" cs))
                    (lambda (cs) (cons "1" cs))))
          (define (iter ns)
            (if (empty? ns)
                ns
                ((vector-ref cv (first ns))
                 (iter (rest ns)))))]
    (iter ns)))
(check-expect (cvts empty) empty)
(check-expect (cvts (list 0 1 0 0)) (list "0" "1" "0" "0"))