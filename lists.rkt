#lang racket

(provide take-while split-while)

(define (any? lst)
  (not (empty? lst)))

(define (split-while f xs)
  (if (and (any? xs) (f (car xs)))
    (let-values
      ([(taken remain) (split-while f (cdr xs))])
      (values (cons (car xs) taken) remain))
    (values empty xs)))

(define (take-while pred lst)
  (match lst
         ['() '()]
         [(cons x xs)
          (if (pred x)
            (cons x (take-while pred xs))
            (take-while pred xs))]))
