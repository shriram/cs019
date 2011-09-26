#lang racket/base

(provide check-violation-highlights)

;; Provides the form check-violation-highlights to see if
;; some expression raises highlights around the expected syntax.

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
  (substring (call-with-input-file* (srcloc-source a-srcloc) port->string)
             (sub1 (srcloc-position a-srcloc))
             (+ (sub1 (srcloc-position a-srcloc))
                (srcloc-span a-srcloc))))

(define-syntax (check-violation-highlights stx)
  (syntax-case stx ()
    [(_ body expected-highlights)
     (syntax/loc stx
       (check-expect (map get-text-at-srcloc
                          (signature-violation-srclocs
                           (with-handlers 
                               ([signature-violation? (lambda (exn) exn)])
                             body)))
                     expected-highlights))]))
