#lang racket

(require srfi/45 "factors.rkt" "streams.rkt")

(define (abundant? x)
  (> (apply + (proper-divisors x)) x))

(define abundant-numbers
  (stream-filter abundant? (in-naturals 1)))

(define (append-lazy eager-list lazy-list)
  (lazy
    (match eager-list
           ['() lazy-list]
           [(cons x xs)
            (delay (cons x (append-lazy xs lazy-list)))])))

(define (lazy->set lz)
  (local
    [(define seed (make-hash))
     (define (grow-seed z)
       (match (force z)
              ['() void]
              [(cons x xs)
               (hash-set! seed x #t)
               (grow-seed xs)]))]
    (grow-seed lz)
    (list->set (hash-keys seed))))

(define (filter-lazy pred lz)
  (lazy
    (match (force lz)
           ['() '()]
           [(cons x xs)
            (if (pred x)
              (delay (cons x (filter-lazy pred xs)))
              (filter-lazy pred xs))])))

(define (sums-with lst x)
  (map (λ(a) (+ x a)) lst))

(define (sums s)
  (lazy
    (match s
           ['() '()]
           [(cons x xs)
            (append-lazy (sums-with s x) (sums xs))])))

(define (abundant-sums-under x)
  (let ((legal? (λ(y) (< y x))))
  (lazy->set
    (filter-lazy
      legal?
      (sums (stream->list (stream-take-while legal? abundant-numbers)))))))

(time
  (define non-abundant-sums
    (set-subtract
      (list->set (stream->list (in-range 1 28124)))
      (abundant-sums-under 28124)))

  (apply + (set->list non-abundant-sums)))
