#lang racket

(require "factors.rkt")

(define (divisor-sums-under n)
  (stream->list
    (stream-map
      (λ(a) (cons a (apply + (proper-divisors a))))
      (in-range 1 n))))

(define (amicable-nums-under n)
  (let ([dsums (make-immutable-hash (divisor-sums-under n))])
    (stream-fold
      (λ(a x)
        (if
          (and
            (eq? x (hash-ref dsums (hash-ref dsums x) 0))
            (not (eq? x (hash-ref dsums x))))
          (cons x a)
          a))
      '()
      (in-range 1 n))))

(apply + (amicable-nums-under 10000))
