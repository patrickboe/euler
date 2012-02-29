#lang racket

(require "strings.rkt" "streams.rkt")

(define (file-contents filename)
  (port->string (open-input-file filename)))

(define (names-string->list s)
  (split (string-remove "\"" s) ","))

(define (alpha-value c)
  (- (char->integer c) 64))

(define (word-value w)
  (sequence-fold + 0 (sequence-map alpha-value w)))

(define (name-scores names)
  (stream-map
    (match-lambda [(cons ord val) (* ord val)])
    (stream-zip
      (in-naturals 1)
      (sequence->stream (map word-value names)))))

(define (list-before? x y)
  (or (empty? x)
    (and (not (empty? y))
      (or (< (car x) (car y))
        (and (equal? (car x) (car y))
             (list-before? (cdr x) (cdr y)))))))

(define (comes-before? f x y)
  (list-before? (f x) (f y)))

(define (word-to-value-list a)
  (map alpha-value (sequence->list a)))

(define (word-alpha-lower? a b)
  (comes-before? word-to-value-list a b))

(define (alpha-sort xs)
  (sort xs word-alpha-lower?))

(stream-fold + 0
  (name-scores
    (alpha-sort
      (names-string->list
        (file-contents "resources/names.txt")))))
