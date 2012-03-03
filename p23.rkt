#lang racket

(require "factors.rkt" "streams.rkt")

(define (abundant? x)
  (> (apply + (proper-divisors x)) x))

(define abundant-numbers
  (stream-filter abundant? (in-naturals 1)))

(define (sum-of-non-abundant-sums-under limit)
  (local
    [(define abunds-list (stream->list
       (stream-take-while (λ(x) (< x limit)) abundant-numbers)))
     (define abunds-set (list->set abunds-list))
     (define (non-abundant-sum? x)
       (not (ormap
              (λ(y) (set-member? abunds-set (- x y)))
              abunds-list)))
     (define non-abundant-sums
       (stream-filter non-abundant-sum?  (in-range 1 limit)))]
    (stream-fold + 0 non-abundant-sums)))

(time (sum-of-non-abundant-sums-under 21824))
