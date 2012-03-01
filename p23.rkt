#lang racket

(require "streams.rkt" "factors.rkt")

(define (abundant? x)
  (> (apply + (proper-divisors x)) x))

(define abundant-numbers
  (stream-filter abundant? (in-naturals 1)))

(define (any? xs)
  (not (empty? xs)))

(define (split-while f xs)
  (if (and (any? xs) (f (car xs)))
    (let-values
      ([(taken remain) (split-while f (cdr xs))])
      (values (cons (car xs) taken) remain))
    (values empty xs)))

(define (pair-list-with xs x)
  (map (λ(a) (cons x a)) xs))

(define pairs
  (match-lambda
    [(cons x xs) (append (pair-list-with (cons x xs) x) (pairs xs))]
    [_ empty]))

(define (zip-pairs from-front from-back ceil)
  (if (empty? from-front)
    empty
    (if (empty? from-back)
      (pairs from-front)
      (if (> (+ (car from-front) (car from-back)) ceil)
        (zip-pairs from-front (cdr from-back) ceil)
        (match-let ([(cons x xs) from-front])
          (append
            (pair-list-with from-front x)
            (pair-list-with from-back x)
            (zip-pairs xs from-back ceil)))))))

(define (ceilinged-pairs xs ceil)
  (let*-values
    ([(half-ceil) (/ ceil 2)]
     [(under-half over-half) (split-while (λ(x) (<= x half-ceil)) xs)])
     (zip-pairs under-half (reverse over-half) ceil)))
