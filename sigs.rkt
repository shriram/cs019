#lang racket/base

(require (only-in lang/htdp-advanced [define asl:define] [lambda asl:lambda]))
(require [for-syntax syntax/parse]
         [for-syntax syntax/struct])
(require syntax/struct)
(require [for-syntax racket])
(require racket/bool racket/list)

(provide define: define-struct: and: or: not:)

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(_ s ([f : C] ...))
     (with-syntax ([(names ...) 
                    (build-struct-names #'s
                                        (syntax->list #'(f ...)) 
                                        #f #f)]
                   [orig stx])
       (with-syntax ([sig-name (datum->syntax #'s
                                              (string->symbol
                                               (string-append
                                                (symbol->string
                                                 (syntax->datum #'s))
                                                "$")))]
                     [cnstr (syntax-case #'(names ...) ()
                              [(struct:name-id constructor misc ...)
                               #'constructor])]
                     [pred (syntax-case #'(names ...) ()
                             [(srtuct:name-id const predicate misc ...)
                              #'predicate])])
         #'(begin
             (define-values (names ...)
               (let ()
                 (begin
                   (define-struct s (f ...) #:transparent #:mutable)
                   (let ([cnstr 
                          (lambda (f ...)
                            (let ([wrapped-args
                                   (let loop ([sigs (list C ... )]
                                              [args (list f ...)]
                                              [n 1])
                                     (if (empty? sigs)
                                         empty
                                         (cons (wrap (first sigs) 
                                                     (first args))
                                               (loop (rest sigs) (rest args) (add1 n)))))])
                              (apply cnstr wrapped-args)))])
                     (values names ...)))))
             ;; This could be a define below, but it's a define-values
             ;; due to a bug in ISL's local.  See users@racket-lang.org
             ;; thread, 2011-09-03, "splicing into local".  Should not
             ;; be necessary with next release.
             (define-values (sig-name) 
               (first-order-sig pred (symbol->string 's))))))]))

(define (wrap sig val)
  ((signature-wrapper sig) val))

(provide Number$ String$ proc: pred->sig Listof:)

(define-struct signature (pred wrapper ho?))

(define (Listof: s)
  (if (signature-ho? s)
      (make-signature list?
                      (lambda (v)
                        (map (lambda (e) (wrap s e)) v))
                      true)
      (let ([pred (lambda (v)
                    (and (list? v)
                         (andmap (signature-pred s) v)))])
        (make-signature pred
                        (lambda (v)
                          (if (pred v)
                              v
                              (error 'signature-violation "~s not a list of ~a"
                                     v
                                     (object-name s))))
                        false))))

(define (first-order-sig pred? descr)
  (make-signature pred?
                  (lambda (v)
                    (if (pred? v)
                        v
                        (error 'signature-violation "~s is not a ~a" v descr)))
                  false))

(define Number$ (first-order-sig number? "number"))
(define String$ (first-order-sig string? "string"))

(define (pred->sig p)
  (first-order-sig p (object-name p)))

(define-syntax (proc: stx)
  (syntax-case stx (->)
    [(_ (a ... -> r))
     (with-syntax ([(args ...) (generate-temporaries #'(a ...))])
       #'(make-signature
          procedure?
          (lambda (v)
            (if (procedure? v)
                (lambda (args ...)
                  (wrap r (v (wrap a args) ...)))
                (error 'signature-violation "~s is not a procedure" v)))
          true))]))

(define-syntax (define: stx)
  (syntax-case stx (: ->)
    [(_ id : C exp)
     (identifier? #'id)
     #'(asl:define id (wrap C exp))]
    [(_ (f [a : Ca] ...) -> Cr exp) 
     #'(asl:define (f a ...)
                   (let ([a (wrap Ca a)] ...)
                     (wrap Cr exp)))]))

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