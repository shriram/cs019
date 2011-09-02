#lang racket

;(require-for-syntax (only-in lang/htdp-advanced [define asl:define]))

(provide define:)

(define-syntax (define: stx)
  (syntax-case stx (:)
    [(_ id : C exp)
     (identifier? #'id)
     #'(define id
         (let ([e exp])
           (if (C e)
               e
               (error 'signature "violation of ~a" C))))]
    [(_ (f [a : Ca] ...) : Cr exp)
     #'(define (f a ...)
         (let loop ([preds (list Ca ...)] [args (list a ...)] [n 1])
           (if (empty? args)
               (let ([v exp])
                 (if (Cr v)
                     v
                     (error 'signature "violation of return signature ~a" Cr)))
               (if ((first preds) (first args))
                   (loop (rest preds) (rest args) (add1 n))
                   (error 'signature "violation of argument signature ~a in position ~a"
                          (first preds) n)))))]))

(define: (f [x : number?] [y : number?]) : number?
  (+ x y))
(define: x : number? 3)
(define: y : string? 3)