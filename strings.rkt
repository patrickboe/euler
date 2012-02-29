#lang racket

(provide split string-remove)

(define (split s delims)
  (remv "" (regexp-split (string-append "[" delims "]") s)))

(define (string-remove to-remove s)
  (regexp-replace* to-remove s ""))
