#lang racket

(require racket/provide) ;; to get provide's filtered-out
(require (prefix-in bsl: 
                    (except-in lang/htdp-advanced : image? require #%module-begin)))
(require (only-in lang/htdp-advanced (#%module-begin bsl:module-begin)))
(provide (filtered-out (lambda (name)
                         (and (regexp-match #rx"^bsl:" name)
                              (regexp-replace #rx"^bsl:" name "")))
                       (all-from-out lang/htdp-advanced)))
(provide (rename-out (top-level #%module-begin)))

(require "open-image-url.rkt")
(provide open-image-url)

(require "sigs.rkt")
(provide define:)

(require 2htdp/image 2htdp/universe)
(provide [all-from-out 2htdp/universe] [all-from-out 2htdp/image])

; From Racket:
(provide ;make-immutable-hash
         hash hash-set hash-update
         hash-iterate-first hash-iterate-next hash-iterate-key hash-iterate-value)
(provide require only-in except-in prefix-in rename-in combine-in planet)
(provide provide all-defined-out all-from-out rename-out except-out 
                 prefix-out struct-out combine-out protect-out)

(require test-engine/scheme-tests) ;; for run-tests and display-results

(define-syntax (top-level body-exprs)
  (syntax-case body-exprs ()
    [(_ bodies ...)
     #'(bsl:module-begin bodies ... (run-tests) (display-results))]))
