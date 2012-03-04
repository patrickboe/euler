#lang lazy

(define (fibo-step a b)
  (let ((c (+ a b)))
    (cons c (fibo-step b c))))

(define fibonacci-sequence
  (cons 1 (fibo-step 0 1)))

(define ln10 (log 10))

(define (log-base-10 x)
  (/ (log x) ln10))

(define (digit-count x)
  (+ 1 (floor (log-base-10 x))))

(define (take-while pred lst)
  (if (pred (first lst))
    (cons (first lst) (take-while pred (rest lst)))
    '()))

(define (first-fibo-with-n-digits n)
  (+ 1 (length (take-while (λ(x) (< (digit-count x) n)) fibonacci-sequence))))
