#lang racket

(provide
  stream-zip
  stream-take
  stream-take-while
  stream-drop
  stream-last
  iterate)

(define (stream-take-while f s)
  (if (and (not (stream-empty? s)) (f (stream-first s)))
    (stream-cons (stream-first s) (stream-take-while f (stream-rest s)))
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
  (if (null? x)
    empty-stream
    (stream-cons x (iterate f (f x)))))

(define (stream-last s)
  (if (stream-empty? (stream-rest s))
    (stream-first s)
    (stream-last (stream-rest s))))

(define (stream-zip a b)
  (if (or (stream-empty? a) (stream-empty? b))
    empty-stream
    (stream-cons
      (cons (stream-first a) (stream-first b))
      (stream-zip (stream-rest a) (stream-rest b)))))
