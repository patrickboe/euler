#lang racket

(provide
  stream-take
  stream-take?
  stream-drop
  iterate)

(define (stream-take? f s)
  (if (f (stream-first s))
    (stream-cons (stream-first s) (stream-take? f (stream-rest s)))
    empty))

(define (stream-drop s n)
  (if (or (eq? 0 n) (stream-empty? s))
    s
    (stream-drop (stream-rest s) (- n 1))))

(define (stream-take s n)
  (if (or (eq? 0 n) (stream-empty? s))
    empty
    (stream-cons (stream-first s) (stream-take (stream-rest s) (- n 1)))))

(define (iterate f x)
  (stream-cons x (iterate f (f x))))
