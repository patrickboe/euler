#lang racket/base

(provide split-while)

(define (split-while f xs)
  (if (and (any? xs) (f (car xs)))
    (let-values
      ([(taken remain) (split-while f (cdr xs))])
      (values (cons (car xs) taken) remain))
    (values empty xs)))

