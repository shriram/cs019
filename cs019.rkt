#lang racket

(require [except-in lang/htdp-advanced require]
         test-engine/scheme-tests
         "open-image-url.rkt")

(provide open-image-url
         make-immutable-hash
         hash
         hash-set
         hash-update
         hash-iterate-first
         hash-iterate-next
         hash-iterate-key
         hash-iterate-value
         require only-in except-in prefix-in rename-in combine-in
         provide all-defined-out all-from-out rename-out except-out prefix-out struct-out combine-out protect-out
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
