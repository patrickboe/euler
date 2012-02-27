#lang racket

(define (! x)
  (sequence-fold * 1 (in-range 1 (+ x 1))))

(foldl + 0
  (map string->number
    (map string
      (sequence->list (number->string (! 100))))))
