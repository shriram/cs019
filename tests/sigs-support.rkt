#lang racket/base

(provide check-violation-highlights)

;; Provides the form check-violation-highlights to see if
;; some expression raises highlights around the expected syntax.
;; Also takes in a list of the column where the syntax starts to
;; highlight.

(require "../impl/sigs.rkt"
         (only-in racket/base 
                  with-handlers
                  call-with-input-file*
                  vector->list
                  srcloc 
                  srcloc-source
                  srcloc-position 
                  srcloc-span
                  define-syntax)
         (only-in racket/port port->string)
         (only-in "../cs019.rkt" check-expect)
         (for-syntax racket/base))

(define (get-text-at-srcloc a-srcloc-vector)
  (define a-srcloc (apply srcloc (vector->list a-srcloc-vector)))
  (substring (regexp-replace* "\r"
                              (call-with-input-file* #:mode 'text
                                                     (srcloc-source a-srcloc)
                                                     port->string)
                              "")
             (sub1 (srcloc-position a-srcloc))
             (+ (sub1 (srcloc-position a-srcloc))
                (srcloc-span a-srcloc))))

(define-syntax (check-violation-highlights stx)
  (syntax-case stx ()
    [(_ body expected-highlights expected-columns)
     (quasisyntax/loc stx
       (begin
         #,(syntax/loc #'expected-highlights
             (check-expect (map get-text-at-srcloc
                                (signature-violation-srclocs
                                 (with-handlers 
                                     ([signature-violation? (lambda (exn) exn)])
                                   body)))
                           expected-highlights))
         #,(syntax/loc #'expected-columns
             (check-expect (map (lambda (v) (vector-ref v 2))
                                (signature-violation-srclocs
                                 (with-handlers 
                                     ([signature-violation? (lambda (exn) exn)])
                                   body)))
                           expected-columns))))]))
