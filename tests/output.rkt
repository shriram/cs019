#lang planet cs019/cs019

;; Run (do-worlds) to test big-bang and animate programs

"Testing printer"
true
'(1 2 3)
(list 1 2 3)
22/7
(vector 1 2 3)
(define-struct f (x))
(make-f 10)
(make-f (make-f 10))
(define-struct g (a b))
(make-g 1 2)

(define i (open-image-url "http://racket-lang.org/logo.png"))
(check-expect (image-height i) 85)
(check-expect (image-width i) 88)

(open-image-url "http://www.continental.com/web/format/img/header/continentalLogo.gif")

(define (draw-circ t)
  (place-image (circle (modulo t 50) "solid" "red")
               50
               50
               (empty-scene 100 100)))

(define (do-worlds)
  (begin
    (big-bang 0 [on-tick add1] 
              [on-draw (lambda (w) 
                         (overlay (circle (modulo w 50) "outline" "red")
                                  (empty-scene 100 100)))])
    (animate draw-circ)))
