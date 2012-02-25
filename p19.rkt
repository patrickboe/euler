#lang racket

(define (tie l) (shared [(c (append l c))] c))

(define days
  (tie '(Monday Tuesday Wednesday Thursday Friday Saturday Sunday)))

(struct month (first? get-length))

(define make-month-calcs (λ month-args
  (let ([day-calcs
    (map
      (λ(m) (if (number? m) (λ(year) m) m))
      month-args)])
    (cons
      (month #t (car day-calcs))
      (map (λ(calc) (month #f calc)) (cdr day-calcs))))))

(define (february-calc year)
  (if (eq? 0 (modulo year 4)) 29 28))

(define months
  (tie (make-month-calcs 31 february-calc 31 30 31 30 31 31 30 31 30 31)))
