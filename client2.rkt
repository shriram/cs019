;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname client2) (read-case-sensitive #t) (teachpacks ((lib "cs0190tpk.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "cs0190tpk.rkt" "installed-teachpacks")))))
(require "client.rkt")

(check-expect (hash-ref ht2 "x") 10)
(check-expect (g-a (make-g 1 2)) 1)

