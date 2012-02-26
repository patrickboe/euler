#lang racket

(define (tie l) (shared [(c (append l c))] c))

(define days
  (tie '(Monday Tuesday Wednesday Thursday Friday Saturday Sunday)))

(struct month (last? get-length))

(define (drop-neq l sought)
  (if (eq? (car l) sought)
    l
    (drop-neq (cdr l) sought)))

(define (stream-take? f s)
  (if (f (stream-first s))
    (stream-cons (stream-first s) (stream-take? f (stream-rest s)))
    empty))

(define make-months (λ month-args
  (let
    ([to-calc (λ(m) (if (number? m) (λ(year) m) m)) ])
    (append
      (map (λ(x) (month #f (to-calc x))) (drop-right month-args 1))
      (list (month #t (to-calc (last month-args))))))))

(define (february-calc year)
  (if (eq? 0 (modulo year 4)) 29 28))

(define months
  (tie (make-months 31 february-calc 31 30 31 30 31 31 30 31 30 31)))

(struct date (dy d m y))

(define (stream-calendar day-seq date-seq month-seq year-seq)
  (let*
    ([m (car month-seq)]
     [d (stream-first date-seq)]
     [y (stream-first year-seq)]
     [changing-month? (eq? ((month-get-length m) y) d)])
    (stream-cons
      (date (car day-seq) d m y)
      (if changing-month?
        (stream-calendar
          (cdr day-seq)
          (in-naturals 1)
          (cdr month-seq)
          (if (month-last? m)
            (stream-rest year-seq)
            year-seq))
        (stream-calendar
          (cdr day-seq) (stream-rest date-seq) month-seq year-seq)))))

(define calendar
  (let*
    ([tues (drop-neq days 'Tuesday)]
     [the-first (in-naturals 1)]
     [jan months]
     [nineteen-oh-one (in-naturals 1901)])
    (stream-calendar tues the-first jan nineteen-oh-one)))

(define twentieth-century-sunday-the-firsts
  (stream-filter
    (λ(d)
      (and
        (eq? 'Sunday (date-dy d))
        (eq? 1 (date-d d))))
    (stream-take? 
      (λ(d) (< (date-y d) 2001))
        calendar)))

(stream-length twentieth-century-sunday-the-firsts)
