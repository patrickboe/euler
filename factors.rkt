#lang racket

(require "streams.rkt")

(define (divides? x y)
  (eq? 0 (modulo x y)))

(define (eliminate x factor)
  (let ([d (/ x factor)])
    (if (not (divides? d factor))
      d
      (eliminate d factor))))

(define (limited-factors-in x coll lim)
  (let ([guess (first coll)])
    (if (divides? x guess)
      (stream-cons guess (limited-factors-in x (rest coll) x))
      (if (or (> guess lim) (empty? (rest coll)))
        empty
        (limited-factors-in x (rest coll) lim)))))

(define (factors-in x coll)
  (limited-factors-in x coll (sqrt x)))

(define (none-are-factors? coll)
  (λ(x) (empty? (factors-in x coll))))

(define (in-series start step)
  (stream-cons start (in-series (+ step start) step)))

(define stream-primes (case-lambda
  [() (stream-append (stream 2 3) (stream-primes '(2 3)))]
  [(known)
   (let*
     ([last-known (last known)]
      [this-one (stream-first (stream-filter
           (none-are-factors? known)
           (in-series (+ 2 last-known) 2)))])
      (stream-cons
        this-one
        (stream-primes (append known (list this-one)))))]))

(define primes (stream-primes))

(define (prime-factors n)
  (let*
    ([limit (sqrt n)]
     [under-limit? (λ(x) (<= x limit))]
     [nonself-factors
       (stream-filter (λ(x) (divides? n x))
         (stream-take? under-limit? primes))])
      (if (stream-empty? nonself-factors)
        (stream n)
        (let ([first-factor (stream-first nonself-factors)])
          (stream-cons
            first-factor
            (prime-factors (/ n first-factor)))))))

(define (include x)
  (λ(combo) (cons x combo)))

(struct combo-step (combos xs))

(define add-combo (match-lambda
  [(combo-step combos xs)
   (if (stream-empty? xs)
     null
     (combo-step
       (append combos (map (include (stream-first xs)) combos))
       (stream-rest xs)))]))

(define (valued? x) (not (null? x)))

(define (combinations xs)
  (combo-step-combos
    (stream-last
      (iterate add-combo (combo-step (list empty) xs)))))
