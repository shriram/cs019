#lang racket/base

#;(require (only-in racket/base
                  [define-struct racket:define-struct]))
(require (only-in lang/htdp-advanced 
                  local
                  [define asl:define] 
                  [lambda asl:lambda]))
(require [for-syntax syntax/struct]
         [for-syntax racket])
(require racket/bool racket/list)

(provide define: lambda: define-struct: and: or: not:
         (struct-out signature-violation))

(define the-undefined-value (letrec ([x x]) x))

(define-struct (signature-violation exn:fail) 
  (srclocs)  ;; listof srcloc-vector
  #:property prop:exn:srclocs
  (lambda (violation)
    (map (lambda (vec)
           (apply srcloc (vector->list vec)))
         (signature-violation-srclocs violation))))


;; syntax-srcloc: syntax -> srcloc-vector
(define-for-syntax (syntax-srcloc stx)
  (vector (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))


(define-for-syntax (parse-sig stx)
  (syntax-case stx (->)
    [(A ... -> R)
     (with-syntax ([(A ...) (map parse-sig (syntax->list #'(A ...)))]
                   [R (parse-sig #'R)])
       (syntax/loc stx
         (proc: (A ... -> R))))]
    [_ stx]))

(define-for-syntax (parse-sigs stxs)
  (map parse-sig (syntax->list stxs)))

(define-syntax (define-struct: stx)
  (syntax-case stx (:)
    [(_ sn ([f : S] ...))
     (with-syntax ([(names ...) 
                    (map (lambda (i)
                           (datum->syntax stx i))
                         (map syntax->datum
                              (build-struct-names #'sn
                                                  (syntax->list #'(f ...)) 
                                                  #f #f)))]
                   [term-srcloc (syntax-srcloc stx)]
                   [(S ...) (parse-sigs #'(S ...))])
       (with-syntax ([(S-srcloc ...) (map syntax-srcloc (syntax->list #'(S ...)))]
                     [sig-name (datum->syntax #'sn
                                              (string->symbol
                                               (string-append
                                                (symbol->string
                                                 (syntax->datum #'sn))
                                                "$")))]
                     [(cnstr pred get/set! ...)
                      (syntax-case #'(names ...) ()
                        [(_s:id constructor predicate? getters/setters! ...)
                         #'(constructor predicate? getters/setters! ...)])])
         (with-syntax ([(setters ...) (let loop ([g/s! (syntax->list #'(get/set! ...))])
                                        (if (empty? g/s!) 
                                            empty
                                            (cons (second g/s!)
                                                  (loop (rest (rest g/s!))))))])
#|
This expansion used to use 
         #'(begin
             (define-values (names ...)
               (let ()
                 (begin
                   (define-struct sn (f ...) #:transparent #:mutable)
                   (let ([cnstr 
                          (lambda (f ...)
                            (let ([wrapped-args ETC])
                              (apply cnstr wrapped-args)))]
                         [setters ETC]
                         ...)
                     (values names ...)))))
        ETC.
It does not because that fails with shared:
(define-struct: foo ([n : Number$]))
(shared ([f (make-foo A)]
         [A 3])
  f)
produces (make-foo #<undefined>) rather than (make-foo 3).
The version below, which mutates the setters, does not suffer from this.
|#
           #'(begin
               (define-struct sn (f ...) #:transparent #:mutable)
               (define dummy
                 (set! cnstr
                       (let ([prim-cnstr cnstr])
                         (lambda (f ...)
                           (let ([wrapped-args
                                  (let loop ([sigs (list S ... )]
                                             [args (list f ...)]
                                             [sig-srclocs (list S-srcloc ...)]
                                             [n 1])
                                    (if (null? sigs)
                                        '()
                                        (cons (wrap (car sigs) 
                                                    (car args)
                                                    (car sig-srclocs))
                                              (loop (cdr sigs) 
                                                    (cdr args)
                                                    (cdr sig-srclocs)
                                                    (add1 n)))))])
                             (apply prim-cnstr wrapped-args))))))
               (define more-dummies
                 (list
                  (set! setters
                        (let ([prim-setter setters])
                          (lambda (struct-inst new-val)
                            (prim-setter struct-inst (wrap S new-val S-srcloc)))))
                  ...))
               ;; This could be a define below, but it's a define-values
               ;; due to a bug in ISL's local.  See users@racket-lang.org
               ;; thread, 2011-09-03, "splicing into local".  Should not
               ;; be necessary with next release.
               (define-values (sig-name) 
                 (first-order-sig pred term-srcloc))))))]))

(define (raise-signature-violation msg srclocs)
  (raise (signature-violation msg (current-continuation-marks) srclocs)))

(define (not-sig-error srcloc)
  (raise-signature-violation "not a valid signature" (list srcloc)))

(define (wrap sig val srcloc)
  (if (signature? sig)
      (if (eq? val the-undefined-value)
          val
          ((signature-wrapper sig) val))
      (not-sig-error srcloc)))

(provide Number$ String$ Char$ Boolean$ Any$ Void$ Sig: Listof: Vectorof:)

(define-struct signature (pred wrapper ho? srcloc))

(define-syntax (Listof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [sig-srcloc (syntax-srcloc #'S)]
                   [term-srcloc (syntax-srcloc stx)])
       #'(let ([s S])
           (if (signature? s)
               (if (signature-ho? s)
                   (make-signature list?
                                   (lambda (v)
                                     (map (lambda (e) (wrap s e sig-srcloc)) v))
                                   #t
                                   term-srcloc)
                   (let ([pred (lambda (v)
                                 (and (list? v)
                                      (andmap (signature-pred s) v)))])
                     (make-signature pred
                                     (lambda (v)
                                       (if (pred v)
                                           v
                                           (if (list? v)
                                               (raise-signature-violation
                                                (format "not an appropriate list: ~e" v)
                                                (list sig-srcloc))
                                               (raise-signature-violation
                                                (format "not a list: ~e" v)
                                                (list term-srcloc)))))
                                     #f
                                     term-srcloc)))
               (not-sig-error sig-srcloc))))]))

(define-syntax (Vectorof: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [sig-srcloc (syntax-srcloc #'S)]
                   [term-srcloc (syntax-srcloc stx)])
       #'(let ([s S])
           (if (signature? s)
               (if (signature-ho? s)
                   (make-signature vector?
                                   (lambda (v)
                                     (list->vector
                                      (map (lambda (e) (wrap s e sig-srcloc))
                                           (vector->list v))))
                                   #t
                                   term-srcloc)
                   (let ([pred (lambda (v)
                                 (and (vector? v)
                                      (andmap (signature-pred s)
                                              (vector->list v))))])
                     (make-signature pred
                                     (lambda (v)
                                       (if (pred v)
                                           v
                                           (if (vector? v)
                                               (raise-signature-violation
                                                (format "not an appropriate vector: ~e" v)
                                                (list sig-srcloc))
                                               (raise-signature-violation
                                                (format "not a vector: ~e" v)
                                                (list term-srcloc)))))
                                     #f
                                     term-srcloc)))
               (not-sig-error sig-srcloc))))]))

(define (first-order-sig pred? term-srcloc)
  (make-signature pred?
                  (lambda (v)
                    (if (pred? v)
                        v
                        (raise-signature-violation
                         (format "value ~s failed the signature" v)
                         (list term-srcloc))))
                  #f
                  term-srcloc))

(define-syntax (Sig: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([Sp (parse-sig #'S)]
                   [term-srcloc (syntax-srcloc stx)])
       (if (eq? #'Sp #'S) ;; currently means S is NOT (... -> ...)
           #'(first-order-sig S term-srcloc)
           #'Sp))]))

(define-syntax (Number$ stx)
  (syntax-case stx (Number$)
    [Number$
     (with-syntax ([term-srcloc (syntax-srcloc stx)])
       #'(first-order-sig number? term-srcloc))]))

(define-syntax (String$ stx)
  (syntax-case stx (String$)
    [String$
     (with-syntax ([term-srcloc (syntax-srcloc stx)])
       #'(first-order-sig string? term-srcloc))]))

(define-syntax (Char$ stx)
  (syntax-case stx (char$)
    [Char$
     (with-syntax ([term-srcloc (syntax-srcloc stx)])
       #'(first-order-sig char? term-srcloc))]))

(define-syntax (Boolean$ stx)
  (syntax-case stx (Boolean$)
    [Boolean$
     (with-syntax ([term-srcloc (syntax-srcloc stx)])
       #'(first-order-sig boolean? term-srcloc))]))

(define-syntax (Any$ stx)
  (syntax-case stx (Any$)
    [Any$
     (with-syntax ([term-srcloc (syntax-srcloc stx)])
       #'(first-order-sig (lambda (_) #t) term-srcloc))]))

(define-syntax (Void$ stx)
  (syntax-case stx (Void$)
    [Void$
     (with-syntax ([term-srcloc (syntax-srcloc stx)])
       #'(first-order-sig void? term-srcloc))]))

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
                   [term-srcloc (syntax-srcloc stx)])
       (with-syntax ([(A-srcloc ...) 
                      (map syntax-srcloc (syntax->list #'(A ...)))]
                     [R-srcloc (syntax-srcloc #'R)])
         #'(make-signature
            procedure?
            (lambda (v)
              (if (procedure? v)
                  (lambda (args ...)
                    (wrap R (v (wrap A args A-srcloc) ...) R-srcloc))
                  (raise-signature-violation
                   (format "not a procedure: ~e" v)
                   (list term-srcloc))))
            #t
            term-srcloc)))]))

(define-syntax (define: stx)
  (syntax-case stx (: ->)
    [(_ id : S exp)
     (identifier? #'id)
     (with-syntax ([S (parse-sig #'S)])
       (with-syntax ([S-srcloc (syntax-srcloc #'S)])
         #'(asl:define id (wrap S exp S-srcloc))))]
    [(_ (f [a : Sa] ...) -> Sr exp)
     (with-syntax ([(Sa ...) (parse-sigs #'(Sa ...))]
                   [Sr (parse-sig #'Sr)])
       #'(asl:define f (lambda: ([a : Sa] ...) -> Sr exp)))]))

(define-syntax (lambda: stx)
  (syntax-case stx (: ->)
    [(_ ([a : Sa] ...) -> Sr exp)
     (with-syntax ([(Sa ...) (parse-sigs #'(Sa ...))]
                   [Sr (parse-sig #'Sr)])
       (with-syntax ([(Sa-srcloc ...) (map syntax-srcloc (syntax->list #'(Sa ...)))]
                     [Sr-srcloc (syntax-srcloc #'Sr)])
         #'(asl:lambda (a ...)
                       (let ([a (wrap Sa a Sa-srcloc)] ...)
                         (wrap Sr exp Sr-srcloc)))))]))     

(define-syntax (or: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))]
                   [term-srcloc (syntax-srcloc stx)])
       (with-syntax ([(S-srcloc ...) 
                      (map syntax-srcloc (syntax->list #'(S ...)))])
         #'(first-order-sig
            (lambda (x)
              (let loop ([sigs (list S ...)]
                         [sig-srclocs (list S-srcloc ...)])
                (if (null? sigs)
                    #f
                    (let ([s (car sigs)])
                      (if (signature? s)
                          (if (signature-ho? s)
                              (raise-signature-violation
                               "or: cannot combine higher-order signature" 
                               (list term-srcloc (signature-srcloc s)))
                              (or ((signature-pred s) x)
                                  (loop (cdr sigs) (cdr sig-srclocs))))
                          (not-sig-error (car sig-srclocs)))))))
            term-srcloc)))]))

(define-syntax (and: stx)
  (syntax-case stx ()
    [(_ S ...)
     (with-syntax ([(S ...) (parse-sigs #'(S ...))]
                   [term-srcloc (syntax-srcloc stx)])
       (with-syntax ([(S-srcloc ...) (map syntax-srcloc (syntax->list #'(S ...)))])
         #'(first-order-sig
            (lambda (x)
              (let loop ([sigs (list S ...)]
                         [sig-srclocs (list S-srcloc ...)])
                (if (null? sigs)
                    #t
                    (let ([s (car sigs)])
                      (if (signature? s)
                          (if (signature-ho? s)
                              (raise-signature-violation
                               "and: cannot combine higher-order signature" 
                               (list term-srcloc (signature-srcloc s)))
                              (and ((signature-pred s) x)
                                   (loop (cdr sigs) (cdr sig-srclocs))))
                          (not-sig-error (car sig-srclocs)))))))
            term-srcloc)))]))

(define-syntax (not: stx)
  (syntax-case stx ()
    [(_ S)
     (with-syntax ([S (parse-sig #'S)]
                   [term-srcloc (syntax-srcloc stx)])
       (with-syntax ([sig-srcloc(syntax-srcloc #'S)])
         #'(let ([s S])
             (if (signature? s)
                 (if (signature-ho? s)
                     (raise-signature-violation
                      "not: cannot negate higher-order signature" 
                      (list term-srcloc))
                     (first-order-sig (lambda (x) (not ((signature-pred s) x))) term-srcloc))
                 (not-sig-error sig-srcloc)))))]))

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