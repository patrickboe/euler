#lang racket

(require "factors.rkt")

(define (divisor-sums-under n)
  (stream->list
    (stream-map
      (λ(a) (apply + (proper-divisors a)))
      (in-range 1 n))))

(define (amicable-nums-under n)
  (let*
    ([dsums (list->vector (cons 0 (divisor-sums-under n)))]
     [amicable? (λ(x)
        (let ([this-sum (vector-ref dsums x)])
          (and
            (> (vector-length dsums) this-sum)
            (eq? x (vector-ref dsums this-sum))
            (not (eq? x this-sum)))))])
    (stream-filter amicable? (in-range 1 n))))

(stream-fold + 0 (amicable-nums-under 10000))
