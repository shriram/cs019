#lang s-exp "cs019.rkt"

(define: x : Number$ 3)
;(define: y : string? 4)
(check-expect x 3)

(define: (g [x : Number$]) -> String$ "x")
(check-expect (g 10) "x")
(check-error (g "x"))

(define: (f [x : Number$] [y : Number$]) -> Number$
  (+ x y))
(check-expect (f 10 12) 22)

(define-struct mt ())
(define mt$ (pred->sig mt?))
(define-struct nd (v l r))
(define nd$ (pred->sig nd?))

(define: (a (t : mt$)) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) 1]))
(check-expect (a (make-mt)) 0)
(check-error (a (make-nd 1 2 3)))
(check-error (a 3))

(define-struct: p ([x : Number$] [y : Number$]))
(define: (h [p : p$]) -> Number$
  (p-x p))
(check-expect (h (make-p 1 2)) 1)
(check-expect (h (make-p 3 4)) 3)

(define n->n (proc: (Number$ -> Number$)))
(define: a1 : n->n add1)
(check-expect (a1 5) 6)
(check-error (a1 "x"))

(define: s2n : (proc: (String$ -> Number$)) string->number)
(check-expect (s2n "123") 123)
(check-error (s2n "xyz")) ;; produces false

(define: (i [f : (proc: (Number$ -> Number$))]) -> Number$
  (f 5))
(check-expect (i add1) 6)
(check-error (i number->string))
(check-error (i string->number))

(define: (j [f : (proc: (String$ String$ String$ -> Number$))]) -> Number$
  (f "12" "34" "56"))
(check-expect (j (lambda (s1 s2 s3)
                   (string->number (string-append s1 s2 s3))))
              123456)
(check-error (j string-append))

(define: (d/dx [f : (proc: (Number$ -> Number$))]) -> (proc: (Number$ -> Number$))
  (local [(define: dx : Number$ 0.0001)]
    (lambda (x)
      (/ (- (f (+ x dx)) (f x))
         dx))))
(check-within ((d/dx (lambda (x) (* x x))) 10) 19 21)
(check-error ((d/dx number->string) 10))

(check-expect (local ([define: x : Number$ 3]) x) 3)
(check-error (local ([define: x : String$ 3]) x))

(check-expect (local ([define: (f [x : Number$]) -> String$
                        (number->string x)])
                (f 10))
              "10")
(check-expect (local ([define: (f [p : (proc: (Number$ -> String$))]) -> String$
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

(define: (n [l : (Listof: (proc: (Number$ -> Number$)))]) -> (Listof: Number$)
  (map (lambda (f) (f 10)) l))
(check-expect (n (list add1 sub1)) (list 11 9))
(check-error (n (list add1 number->string)))
(check-error (n (list add1 string->number)))

