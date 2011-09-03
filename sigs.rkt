#lang racket

(require (only-in lang/htdp-advanced [define asl:define] [lambda asl:lambda]))
(require [for-syntax syntax/parse]
         [for-syntax syntax/struct])
(require syntax/struct)
(require [for-syntax racket])

(provide define: define-struct: and: or: not:)

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(_ s ([f : C] ...))
     (with-syntax ([(names ...) 
                    (build-struct-names #'s
                                        (syntax->list #'(f ...)) 
                                        #f #f)]
                   [orig stx])
       (with-syntax ([cnstr (syntax-case #'(names ...) ()
                              [(struct:name-id constructor misc ...)
                               #'constructor])])
         #'(define-values (names ...)
             (let ()
               (begin
                 (define-struct s (f ...) #:transparent #:mutable)
                 (let ([cnstr (lambda args
                                (display "constructing")
                                (newline)
                                (apply cnstr args))])
                   (values names ...)))))))]))

(define-syntax (define: stx)
  (syntax-case stx (:)
    [(_ id : C exp)
     (identifier? #'id)
     #'(asl:define id
         (let ([e exp])
           (if (C e)
               e
               (error 'signature "violation of ~a" C))))]
    [(_ (f [a : Ca] ...) : Cr exp) 
     ;; Assumes only one body term.
     ;; Use of asl:define doesn't automatically ensure this because binding
     ;; v to exp ... would require a begin, which masks the # of terms.
     #'(asl:define (f a ...)
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

(define-syntax (or: stx)
  (syntax-case stx ()
    [(_ p ...)
     #'(lambda (x)
         (let loop ([preds (list p ...)])
           (if (empty? preds)
               false
               (or ((first preds) x)
                   (loop (rest preds))))))]))

(define-syntax (and: stx)
  (syntax-case stx ()
    [(_ p ...)
     #'(lambda (x)
         (let loop ([preds (list p ...)])
           (if (empty? preds)
               true
               (and ((first preds) x)
                    (loop (rest preds))))))]))

(define (not: p)
  (lambda (x) (not (p x))))

#|
(provide : defvar:)

(define-syntax (: stx) (raise-syntax-error stx ': "Cannot be used outside ..."))

(define-syntax (defvar: stx)
  (syntax-parse stx #:literals(:)
    [(_ i:id : C:expr b:expr)
     #'(asl:define i
         (let ([e b])
           (if (C e)
               e
               (error 'signature "violation of ~a" C))))]))
|#