#lang racket/base

(require srfi/45 racket/match)

;==========================================================
;From the tests of the srfi reference implementation
(define (stream-drop s index)
  (lazy
     (if (zero? index)
       s
       (stream-drop (cdr (force s)) (- index 1)))))

(define (from n)
    (delay (cons n (from (+ n 1)))))

(define (stream-filter p? s)
  (lazy (match (force s)
    ('()      (delay '())) 
    ((cons h t) (if (p? h)
      (delay (cons h (stream-filter p? t)))
      (stream-filter p? t))))))

(define (stream-ref s index)
  (lazy
   (match (force s)
     ('()      'error)
     ((cons h t) (if (zero? index)
       h
       (stream-ref t (- index 1)))))))
