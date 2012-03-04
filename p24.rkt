#lang racket

(require "lists.rkt")

(define (lex-permutations lst)
  (if (empty? (cdr lst))
    (list lst)
    (apply append
      (map (λ(a)
        (map (λ(l) (cons a l))
          (lex-permutations (remove a lst))))
      lst))))
