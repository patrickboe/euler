#lang racket

(require srfi/45)

(provide append-lazy lazy->set filter-lazy)

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
