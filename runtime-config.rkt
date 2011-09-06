#lang racket/base

; Uses collects/htdp/bsl/runtime.rkt from nightly build ~2011-09-05
; combined with patch supplied by Robby, 2011-09-01 ("image snips and pretty-print")

(require mzlib/pconvert
         racket/pretty
         lang/private/set-result
         mrlib/image-core
         racket/snip
         racket/class)

(provide configure)

(define (configure options)
  ;; Set print-convert options:
  (booleans-as-true/false #t)
  (constructor-style-printing #t)
  (abbreviate-cons-as-list (memq 'abbreviate-cons-as-list options))
  (current-print-convert-hook
   (let ([ph (current-print-convert-hook)])
     (lambda (val basic sub)
       (cond
        [(equal? val set!-result) '(void)]
        [(is-image? val) val]
        [else (ph val basic sub)]))))
  (use-named/undefined-handler
   (lambda (x)
     (and (memq 'use-function-output-syntax options)
          (procedure? x)
          (object-name x))))
  (named/undefined-handler
   (lambda (x)
     (string->symbol
      (format "function:~a" (object-name x)))))
  ;; Set pretty-print options:
  (pretty-print-show-inexactness #t)
  (pretty-print-exact-as-decimal #t)
  (define img-str "#<image>")
  (define (is-image? val)
    (or (is-a? val image%)         ;; 2htdp/image
        (is-a? val image-snip%)))  ;; literal image constant
  (show-sharing (memq 'show-sharing options))

  ;; Set print handlers to use print-convert and pretty-write:
  (define (set-handlers thunk)
    (parameterize ([pretty-print-print-hook
                    (let ([oh (pretty-print-print-hook)])
                      (lambda (val display? port)
                        (cond
                          [(is-image? val)
                           (if (port-writes-special? port)
                               (write-special val port)
                               (display img-str port))]
                          [else
                           (oh val display? port)])))]
                   [pretty-print-size-hook
                    (let ([oh (pretty-print-size-hook)])
                      (lambda (val display? port)
                        (cond
                          [(is-image? val)
                           (if (port-writes-special? port)
                               1
                               (string-length img-str))]
                          [else
                           (oh val display? port)])))])
      (thunk)))
  (current-print
   (lambda (v)
     (unless (void? v)
       (define converted (print-convert v))
       (set-handlers
        (λ () (pretty-write converted))))))
  (let ([orig (global-port-print-handler)])
    (global-port-print-handler
     (lambda (val port [depth 0])
       (parameterize ([global-port-print-handler orig])
         (let ([val (print-convert val)])
           (parameterize ([pretty-print-columns 'infinity])
             (set-handlers
              (λ () (pretty-write val port))))))))))