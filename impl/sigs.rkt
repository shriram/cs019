#lang racket/base

(require (only-in lang/htdp-advanced [define asl:define] [lambda asl:lambda]))
(require [for-syntax syntax/parse]
         [for-syntax syntax/struct])
(require syntax/struct)
(require [for-syntax racket])
(require racket/bool racket/list)

(provide define: lambda: define-struct: and: or: not: proc:)

(define-for-syntax (parse-sig stx)
  (syntax-case stx (->)
    [(A ... -> R)
     (with-syntax ([(A ...) (map parse-sig (syntax->list #'(A ...)))]
                   [R (parse-sig #'R)])
       #'(proc: (A ... -> R)))]
    [_ stx]))

(define-for-syntax (parse-sigs stxs)
  (map parse-sig (syntax->list stxs)))

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(_ sn ([f : S] ...))
     (with-syntax ([(names ...) 
                    (build-struct-names #'sn
                                        (syntax->list #'(f ...)) 
                                        #f #f)]
                   [orig stx]
                   [(S ...) (parse-sigs #'(S ...))])
       (with-syntax ([sig-name (datum->syntax #'sn
                                              (string->symbol
                                               (string-append
                                                (symbol->string
                                                 (syntax->datum #'sn))
                                                "$")))]
                     [cnstr (syntax-case #'(names ...) ()
                              [(struct:name-id constructor misc ...)
                               #'constructor])]
                     [(_sid _ctr _id? setters ...)
                      (build-struct-names #'sn
                                          (syntax->list #'(f ...))
                                          #t #f)]
                     [pred (syntax-case #'(names ...) ()
                             [(struct:name-id const predicate misc ...)
                              #'predicate])])
         #'(begin
             (define-values (names ...)
               (let ()
                 (begin
                   (define-struct sn (f ...) #:transparent #:mutable)
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
                              (apply cnstr wrapped-args)))]
                         [setters
                          (lambda (struct-inst new-val)
                            (setters struct-inst (wrap S new-val)))]
                         ...)
                     (values names ...)))))
             ;; This could be a define below, but it's a define-values
             ;; due to a bug in ISL's local.  See users@racket-lang.org
             ;; thread, 2011-09-03, "splicing into local".  Should not
             ;; be necessary with next release.
             (define-values (sig-name) 
               (first-order-sig pred)))))]))

(define (wrap sig val)
  ((signature-wrapper sig) val))

(provide Number$ String$ Boolean$ Any$ pred->sig Listof: Vectorof:)

(define-struct signature (pred wrapper ho?))

(define-syntax (Listof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)])
       #'(make-listof-sig S))]))

(define (make-listof-sig s) ;; s has already been parsed
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

(define-syntax (Vectorof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)])
       #'(make-vectorof-sig S))]))

(define (make-vectorof-sig s)  ;; s has already been parsed
  (if (signature-ho? s)
      (make-signature vector?
                      (lambda (v)
                        (list->vector
                         (map (lambda (e) (wrap s e))
                              (vector->list v))))
                      true)
      (let ([pred (lambda (v)
                    (and (vector? v)
                         (andmap (signature-pred s)
                                 (vector->list v))))])
        (make-signature pred
                        (lambda (v)
                          (if (pred v)
                              v
                              (error 'signature-violation "~s not a vector of ~a"
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
(define Boolean$ (first-order-sig (lambda (v) (or (eq? v true) (eq? v false)))))
(define Any$ (first-order-sig (lambda (_) true)))

(define (pred->sig p)
  (first-order-sig p))

;; proc: can be used liberally, but it is intended only for users to be able
;; to define stand-alone procedural signatures: eg,
;; (define n->n (proc: (Number$ -> Number$)))
;; In all other cases, the macros invoke parse-sig, which takes care of
;; automatically wrapping (proc: ...) around procedure signatures.
(define-syntax (proc: stx)
  (syntax-case stx (->)
    [(_ (A ... -> R))
     (with-syntax ([(args ...) (generate-temporaries #'(A ...))]
                   [(A ...) (parse-sigs #'(A ...))]
                   [R (parse-sig #'R)])
       #'(make-signature
          procedure?
          (lambda (v)
            (if (procedure? v)
                (lambda (args ...)
                  (wrap R (v (wrap A args) ...)))
                (error 'signature-violation "~s is not a procedure" v)))
          true))]))

(define-syntax (define: stx)
  (syntax-case stx (: ->)
    [(_ id : S exp)
     (identifier? #'id)
     (with-syntax ([S (parse-sig #'S)])
       #'(asl:define id (wrap S exp)))]
    [(_ (f [a : Sa] ...) -> Sr exp)
     (with-syntax ([(Sa ...) (parse-sigs #'(Sa ...))]
                   [Sr (parse-sig #'Sr)])
       #'(asl:define f (lambda: ([a : Sa] ...) -> Sr exp)))]))

(define-syntax (lambda: stx)
  (syntax-case stx (: ->)
    [(_ ([a : Sa] ...) -> Sr exp)
     (with-syntax ([(Sa ...) (parse-sigs #'(Sa ...))]
                   [Sr (parse-sig #'Sr)])
       #'(asl:lambda (a ...)
                     (let ([a (wrap Sa a)] ...)
                       (wrap Sr exp))))]))     

(define-syntax (or: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))])
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
                            (loop (rest sigs))))))))))]))

(define-syntax (and: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))])
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
                             (loop (rest sigs))))))))))]))

(define-syntax (not: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)])
       #'(make-not-sig S))]))

(define (make-not-sig s)  ;; s is already parsed
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