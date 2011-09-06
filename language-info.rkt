#lang racket/base

(provide get-info)

(define ((get-info options) key default)
  (case key
    [(configure-runtime) `(#((planet cs019/cs019/runtime-config) configure ,options))]
    [else default]))
