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
(define-struct nd (v l r))

#;(define: (tf (t : (or: mt? nd?))) -> Number$
  (cond
    [(mt? t) 0]
    [(nd? t) 1]))

;(defvar: y : number? 4)
(define-struct: p ([x : Number$] [y : Number$]))
(define: (h [p : p$]) -> Number$
  (p-x p))
(check-expect (h (make-p 1 2)) 1)
(check-expect (h (make-p 3 4)) 3)

(define n->n (proc$ (Number$ -> Number$)))
(define: a1 : n->n add1)
(check-expect (a1 5) 6)
(check-error (a1 "x"))

(define: s2n : (proc$ (String$ -> Number$)) string->number)
(check-expect (s2n "123") 123)
(check-error (s2n "xyz")) ;; produces false

(define: (i [f : (proc$ (Number$ -> Number$))]) -> Number$
  (f 5))
(check-expect (i add1) 6)
(check-error (i number->string))
(check-error (i string->number))

(define: (j [f : (proc$ (String$ String$ String$ -> Number$))]) -> Number$
  (f "12" "34" "56"))
(check-expect (j (lambda (s1 s2 s3)
                   (string->number (string-append s1 s2 s3))))
              123456)
(check-error (j string-append))