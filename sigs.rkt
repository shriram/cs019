#lang racket/base

(require (only-in lang/htdp-advanced [define asl:define] [lambda asl:lambda]))
(require [for-syntax syntax/parse]
         [for-syntax syntax/struct])
(require syntax/struct)
(require [for-syntax racket])
(require racket/bool racket/list)

(provide define: lambda: define-struct: and: or: not:)

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(_ s ([f : S] ...))
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
                                   (let loop ([sigs (list S ... )]
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
               (first-order-sig pred)))))]))

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

(define (first-order-sig pred?)
  (make-signature pred?
                  (lambda (v)
                    (if (pred? v)
                        v
                        (error 'signature-violation "~s is not a ~a" 
                               v
                               (object-name pred?))))
                  false))

(define Number$ (first-order-sig number?))
(define String$ (first-order-sig string?))

(define (pred->sig p)
  (first-order-sig p))

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
    [(_ id : S exp)
     (identifier? #'id)
     #'(asl:define id (wrap S exp))]
    [(_ (f [a : Sa] ...) -> Sr exp) 
     #'(asl:define f (lambda: ([a : Sa] ...) -> Sr exp))]))

(define-syntax (lambda: stx)
  (syntax-case stx (: ->)
    [(_ ([a : Sa] ...) -> Sr exp)
     #'(asl:lambda (a ...)
                   (let ([a (wrap Sa a)] ...)
                     (wrap Sr exp)))]))     

(define-syntax (or: stx)
  (syntax-case stx ()
    [(_ S ...)
     #'(first-order-sig
        (lambda (x)
          (let loop ([sigs (list S ...)])
            (if (empty? sigs)
                false
                (let ([s (first sigs)])
                  (if (signature-ho? s)
                      (error 'signature-violation 
                             "or: cannot combine higher-order signatures such as ~s" 
                             (object-name s))
                      (or ((signature-pred s) x)
                          (loop (rest sigs)))))))))]))

(define-syntax (and: stx)
  (syntax-case stx ()
    [(_ S ...)
     #'(first-order-sig
        (lambda (x)
          (let loop ([sigs (list S ...)])
            (if (empty? sigs)
                true
                (let ([s (first sigs)])
                  (if (signature-ho? s)
                      (error 'signature-violation 
                             "or: cannot combine higher-order signatures such as ~s" 
                             (object-name s))
                      (and ((signature-pred s) x)
                           (loop (rest sigs)))))))))]))

(define (not: s)
  (if (signature-ho? s)
      (error 'signature-violation
             "not: cannot negate higher-order signatures such as ~s"
             (object-name s))
      (first-order-sig (lambda (x) (not ((signature-pred s) x))))))

#|
(provide : defvar:)

(define-syntax (: stx) (raise-syntax-error stx ': "Cannot be used outside ..."))

(define-syntax (defvar: stx)
  (syntax-parse stx #:literals(:)
    [(_ i:id : S:expr b:expr)
     #'(asl:define i
         (let ([e b])
           (if (S e)
               e
               (error 'signature "violation of ~a" S))))]))
|#