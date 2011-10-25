#lang racket/base

(require [for-syntax racket/base])
(require racket/provide) ;; to get provide's filtered-out
(require
 (prefix-in 
  asl: 
  (except-in lang/htdp-advanced
             memq
             first rest second third fourth fifth sixth seventh eighth
             image? require #%module-begin
             signature : Number Real Rational Integer Natural
             Boolean True False String Empty-list Any Property 
             predicate one-of mixed -> combined Symbol ; list-of
             for-all expect expect-within expect-member-of expect-range ==>)))
(require (only-in lang/htdp-advanced (#%module-begin asl:module-begin)))
(provide (filtered-out (lambda (name)
                         (and (regexp-match #rx"^asl:" name)
                              (regexp-replace #rx"^asl:" name "")))
                       (all-from-out lang/htdp-advanced)))
(provide (rename-out (top-level #%module-begin)))

; This is here because memq in ASL is broken:
; returns non-Boolean answers, which fail in cond predicate positions
(define (boolean-memq v l)
  (if (memq v l)
      #t
      #f))
(provide [rename-out [boolean-memq memq]])

(require "impl/lists.rkt")
(provide [all-from-out "impl/lists.rkt"])

(define Posn$ (Sig: asl:posn?))
(provide Posn$)
(define Key$ String$)
(provide Key$)

(require "impl/open-image-url.rkt")
(provide open-image-url)

(require "impl/sigs.rkt")
(provide [all-from-out "impl/sigs.rkt"])
(provide Image$)
(define Image$ (Sig: image?))

(require 2htdp/image 2htdp/universe)
(provide [all-from-out 2htdp/image] [all-from-out 2htdp/universe])

; From Racket:
(provide ;make-immutable-hash
         hash hash-set hash-update
         hash-iterate-first hash-iterate-next hash-iterate-key hash-iterate-value)
(provide require only-in except-in prefix-in rename-in combine-in planet)
(provide provide all-defined-out all-from-out rename-out except-out 
                 prefix-out struct-out combine-out protect-out)

(require [only-in test-engine/scheme-tests run-tests display-results])

(define-syntax (top-level body-exprs)
  (syntax-case body-exprs ()
    [(_ bodies ...)
     #'(asl:module-begin bodies ... (run-tests) (display-results))]))