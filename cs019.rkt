#lang racket

(require [except-in lang/htdp-advanced require]
         test-engine/scheme-tests
         "open-image-url.rkt")

(provide provide
         open-image-url
         require only-in except-in prefix-in rename-in combine-in only-meta-in for-syntax for-template for-label for-meta
         [except-out (all-from-out lang/htdp-advanced) #%module-begin]
         [rename-out (top-level #%module-begin)])

(require mzlib/pconvert)
(constructor-style-printing true)
(install-converting-printer)
(print-as-expression false)
(abbreviate-cons-as-list false)

(define-syntax (top-level body-exprs)
  (syntax-case body-exprs ()
    [(_ bodies ...)
     #'(#%module-begin bodies ... (run-tests) (display-results))]))
