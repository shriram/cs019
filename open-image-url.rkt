#lang racket/base

(require net/url
         racket/list
         racket/file
         racket/path
         racket/port
         racket/class
         racket/gui/base
         racket/contract)

(define (open-image-url a-url-string)
  (let* ([url (string->url a-url-string)]
         [ip (get-pure-port url)]
         [filename (path/param-path (last (url-path url)))]
         [temp-file-path (normalize-path (make-temporary-file (format "~~a-~a" filename)))])
    (dynamic-wind
     (lambda ()
       (copy-port ip (open-output-file temp-file-path #:exists 'truncate)))
     (lambda ()
       (make-object image-snip% (make-object bitmap% temp-file-path)))
     (lambda ()
       (delete-file temp-file-path)))))
          
(provide/contract [open-image-url (string? . -> . (is-a?/c image-snip%))])