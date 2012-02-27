#lang racket

(define (split s delims)
  (remv "" (regexp-split (string-append "[" delims "]") s)))

(define (lines s) (split s "\n"))

(define (reduce f l) (foldl f (car l) (cdr l)))

(define (repeat n x) (if (eq? n 0) empty (cons x (repeat (- n 1) x))))

(define (has? l n)
  (or
    (eq? n 0)
    (and
      (not (empty? l))
      (has? (cdr l) (- n 1)))))

(define (drop-max l n)
  (if (has? l n) (drop l n) empty))

(define (take-max l n)
  (if (has? l n) (take l n) l))

(define by
  (case-lambda
    [(n l)
     (by n n l)]
    [(n step l) 
     (if (empty? l)
       empty
       (cons (take-max l n) (by n step (drop-max l step))))]))

(define (collate la lb)
  (if
    (empty? la)
      empty
      (cons
        (cons (car la) (car lb))
        (collate (cdr la) (cdr lb)))))

(define (max-path-sum triangle-string)
  (let*
    ([line-to-nums (λ(s) (map string->number (split s " ")))]
     [rows (map line-to-nums (lines triangle-string))]
     [max-sum (match-lambda [(cons candidates x)
       (+ (apply max candidates) x)])]
     [max-sums (λ(lower-list upper-list)
       (map max-sum
         (collate
           (cons (list (car upper-list)) (by 2 1 upper-list))
           lower-list)))])
    (apply max (reduce max-sums rows))))

(max-path-sum
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")
