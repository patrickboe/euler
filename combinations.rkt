#lang racket

(require "streams.rkt")

(provide combinations)

(define (repeat x n)
  (build-list n (λ(a) x)))

(define (take-while f s)
  (if (and (not (empty? s)) (f (first s)))
    (cons (first s) (take-while f (rest s)))
    empty))

(define (include n x)
  (λ(combo) (append combo (repeat x n))))

(struct combo-step (combos xs))

(define (split-on-duplicates xs)
  (let* (
    [start (first xs)]
    [duplicates (take-while (λ(x) (eq? start x)) xs)])
    (values duplicates (drop xs (length duplicates)))))

(define (one-through n)
  (in-range 1 (+ 1 n)))

(define (expand-combos combos duplicates)
  (let ([x (first duplicates)])
    (foldl
      (λ(i c) (append c (map (include i x) combos)))
      combos
      (stream->list (one-through (length duplicates))))))

(define combo-additions (match-lambda
  [(combo-step combos xs)
   (if (empty? xs)
     null
     (let-values ([(these others) (split-on-duplicates xs)])
       (combo-step
         (expand-combos combos these)
         others)))]))

(define (combinations xs)
  (combo-step-combos
    (stream-last
      (iterate combo-additions (combo-step (list empty) xs)))))
