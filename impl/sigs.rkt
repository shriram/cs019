#lang racket/base

(require (only-in lang/htdp-advanced [define asl:define] [lambda asl:lambda]))
(require [for-syntax syntax/parse]
         [for-syntax syntax/struct])
(require syntax/struct)
(require [for-syntax racket])
(require racket/bool racket/list)

(provide define: lambda: define-struct: and: or: not:)

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
                   [term stx]
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
                                              [sig-srcs (syntax->list #'(S ...))]
                                              [n 1])
                                     (if (empty? sigs)
                                         empty
                                         (cons (wrap (first sigs) 
                                                     (first args)
                                                     (first sig-srcs))
                                               (loop (rest sigs) 
                                                     (rest args)
                                                     (rest sig-srcs)
                                                     (add1 n)))))])
                              (apply cnstr wrapped-args)))]
                         [setters
                          (lambda (struct-inst new-val)
                            (setters struct-inst (wrap S new-val #'S)))]
                         ...)
                     (values names ...)))))
             ;; This could be a define below, but it's a define-values
             ;; due to a bug in ISL's local.  See users@racket-lang.org
             ;; thread, 2011-09-03, "splicing into local".  Should not
             ;; be necessary with next release.
             (define-values (sig-name) 
               (first-order-sig pred #'term)))))]))

(define (not-sig-error src)
  (raise-syntax-error 'signature-violation "not a valid signature" src))

(define (wrap sig val src)
  (if (signature? sig)
      ((signature-wrapper sig) val)
      (not-sig-error src)))

(provide Number$ String$ Boolean$ Any$ Sig: Listof: Vectorof:)

(define-struct signature (pred wrapper ho? src))

(define-syntax (Listof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term stx])
       #'(make-listof-sig S #'S #'term))]))

(define (make-listof-sig s sig-src term-src) ;; s has already been parsed
  (if (signature? s)
      (if (signature-ho? s)
          (make-signature list?
                          (lambda (v)
                            (map (lambda (e) (wrap s e sig-src)) v))
                          true
                          term-src)
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
                            false
                            term-src)))
      (not-sig-error sig-src)))

(define-syntax (Vectorof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term stx])
       #'(make-vectorof-sig S #'S #'term))]))

(define (make-vectorof-sig s sig-src term-src) ;; s has already been parsed
  (if (signature? s)
      (if (signature-ho? s)
          (make-signature vector?
                          (lambda (v)
                            (list->vector
                             (map (lambda (e) (wrap s e sig-src))
                                  (vector->list v))))
                          true
                          term-src)
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
                            false
                            term-src)))
      (not-sig-error sig-src)))

(define (first-order-sig pred? term-src)
  (make-signature pred?
                  (lambda (v)
                    (if (pred? v)
                        v
                        (error 'signature-violation "~s is not a ~a" 
                               v
                               (object-name pred?))))
                  false
                  term-src))

(define-syntax (Sig: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([Sp (parse-sig #'S)]
                   [term stx])
       (if (eq? #'Sp #'S) ;; currently means S is (... -> ...)
           #'(first-order-sig S #'term)
           #'Sp))]))

(define-syntax Number$
  (syntax-id-rules ()
    [_ (Sig: number?)]))

(define-syntax String$
  (syntax-id-rules ()
    [_ (Sig: string?)]))

(define-syntax Boolean$
  (syntax-id-rules ()
    [_ (Sig: boolean?)]))

(define-syntax Any$
  (syntax-id-rules ()
    [_ (Sig: (lambda (_) true))]))

;; proc: is for internal use only.
;; Stand-alone procedural signatures are defined using Sig:; e.g.,
;; (define n->n (Sig: (Number$ -> Number$)))
;; In all other cases, the macros invoke parse-sig, which takes care of
;; automatically wrapping (proc: ...) around procedure signatures.
(define-syntax (proc: stx)
  (syntax-case stx (->)
    [(_ (A ... -> R))
     (with-syntax ([(args ...) (generate-temporaries #'(A ...))]
                   [(A ...) (parse-sigs #'(A ...))]
                   [R (parse-sig #'R)]
                   [term stx])
       #'(make-signature
          procedure?
          (lambda (v)
            (if (procedure? v)
                (lambda (args ...)
                  (wrap R (v (wrap A args #'A) ...) #'R))
                (error 'signature-violation "~s is not a procedure" v)))
          true
          #'term))]))

(define-syntax (define: stx)
  (syntax-case stx (: ->)
    [(_ id : S exp)
     (identifier? #'id)
     (with-syntax ([S (parse-sig #'S)])
       #'(asl:define id (wrap S exp 'S)))]
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
                     (let ([a (wrap Sa a #'Sa)] ...)
                       (wrap Sr exp #'Sr))))]))     

(define-syntax (or: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))]
                   [term stx])
       #'(first-order-sig
          (lambda (x)
            (let loop ([sigs (list S ...)]
                       [sig-srcs (syntax->list #'(S ...))])
              (if (empty? sigs)
                  false
                  (let ([s (first sigs)])
                    (if (signature? s)
                        (if (signature-ho? s)
                            (error 'signature-violation 
                                   "or: cannot combine higher-order signatures such as ~s" 
                                   (object-name s))
                            (or ((signature-pred s) x)
                                (loop (rest sigs) (rest sig-srcs))))
                        (not-sig-error (first sig-srcs)))))))
          #'term))]))

(define-syntax (and: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))]
                   [term stx])
       #'(first-order-sig
          (lambda (x)
            (let loop ([sigs (list S ...)]
                       [sig-srcs (syntax->list #'(S ...))])
              (if (empty? sigs)
                  true
                  (let ([s (first sigs)])
                    (if (signature? s)
                        (if (signature-ho? s)
                            (error 'signature-violation 
                                   "or: cannot combine higher-order signatures such as ~s" 
                                   (object-name s))
                            (and ((signature-pred s) x)
                                 (loop (rest sigs) (rest sig-srcs))))
                        (not-sig-error (first sig-srcs)))))))
          #'term))]))

(define-syntax (not: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term stx])
       #'(make-not-sig S #'S #'term))]))

(define (make-not-sig s sig-src term-src)  ;; s is already parsed
  (if (signature? s)
      (if (signature-ho? s)
          (error 'signature-violation
                 "not: cannot negate higher-order signatures such as ~s"
                 (object-name s))
          (first-order-sig (lambda (x) (not ((signature-pred s) x))) term-src))
      (not-sig-error sig-src)))

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