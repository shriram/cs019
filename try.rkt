#lang s-exp "cs019.rkt"

(define: (g [x : Number$]) : Number$ "x")
;(g 10)
;(g "x")

(define: (f [x : Number$] [y : Number$]) : Number$
  (+ x y))
(define: x : Number$ 3)
;(define: y : string? 4)

(check-expect (f 10 12) 22)

(define-struct mt ())
(define-struct nd (v l r))

#;(define: (tf (t : (or: mt? nd?))) : number?
  (cond
    [(mt? t) 0]
    [(nd? t) 1]))

;(defvar: y : number? 4)
(define-struct m ())
(define-struct n (x))
(make-m)
(make-n 5)
(m? (make-n 5))
(n? (make-n 5))
(define-struct: p ([x : Number$] [y : Number$]))
(define: (h [p : p$]) : Number$
  (p-x p))
(h (make-p 1 2))
(h (make-p 3 4))
