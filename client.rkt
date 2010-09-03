;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname client) (read-case-sensitive #t) (teachpacks ((lib "cs0190tpk.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "cs0190tpk.rkt" "installed-teachpacks")))))
(require (only-in racket/base list->vector))
(require racket/tcp)
(provide (all-defined-out))

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

(check-expect (build-list 5 add1) (list 1 2 3 4 5))

(define i (open-image-url "http://racket-lang.org/logo.png"))
(check-expect (image-height i) 85)
(check-expect (image-width i) 88)

(define ht (hash))
(define ht2 (hash-set ht "x" 10))
(check-expect(hash-ref ht2 "x") 10)
(check-error (hash-ref ht "x") "hash-ref: no value found for key: \"x\"")

(define cont (open-image-url "http://www.continental.com/web/format/img/header/continentalLogo.gif"))

(define (draw-circ t)
  (place-image (circle (modulo t 50) "solid" "red")
               50
               50
               (empty-scene 100 100)))

(define (do-worlds)
  (begin
    (big-bang 0 [on-tick add1] 
              [on-draw (lambda (w) (overlay (circle (modulo w 50) "outline" "red") (empty-scene 100 100)))])
    (animate draw-circ)))