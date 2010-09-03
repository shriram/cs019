#lang racket

(require 2htdp/image
         2htdp/universe)

(provide make-immutable-hash
         hash
         hash-set
         hash-update
         hash-iterate-first
         hash-iterate-next
         hash-iterate-key
         hash-iterate-value
         require only-in except-in prefix-in rename-in combine-in
         provide all-defined-out all-from-out rename-out except-out prefix-out combine-out protect-out
         [all-from-out 2htdp/universe]
         [all-from-out 2htdp/image])

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