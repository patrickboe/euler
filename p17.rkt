#lang racket

(define num-names
#hash((1 . "one")
      (2 . "two")
      (3 . "three")
      (4 . "four")
      (5 . "five")
      (6 . "six")
      (7 . "seven")
      (8 . "eight")
      (9 . "nine")
      (10 . "ten")
      (11 . "eleven")
      (12 . "twelve")
      (13 . "thirteen")
      (14 . "fourteen")
      (15 . "fifteen")
      (16 . "sixteen")
      (17 . "seventeen")
      (18 . "eighteen")
      (19 . "nineteen")
      (20 . "twenty")
      (30 . "thirty")
      (40 . "forty")
      (50 . "fifty")
      (60 . "sixty")
      (70 . "seventy")
      (80 . "eighty")
      (90 . "ninety")))

(define (subtwenty? x) (< x 20))

(define (tens? x) (< x 100))

(define (hundreds? x) (< x 1000))

(struct qr (quot rem))

(define (divider x)
  (λ (n)
     (let-values ([(a b) (quotient/remainder n x)])
       (qr a b))))

(define-match-expander subtwenty
  (λ (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(? subtwenty? elts ...)])))

(define-match-expander tens
  (λ (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(? tens? (app (divider 10) (qr elts ...)))])))

(define-match-expander hundreds
  (λ (stx)
    (syntax-case stx ()
      [(_ elts ...)
       #'(? hundreds? (app (divider 100) (qr elts ...)))])))

(define (name n)
  (string-join
    (match n
      [(subtwenty d) (list (hash-ref num-names d))]
      [(tens t d)
       (cons
         (hash-ref num-names (* 10 t))
         (if (= d 0) empty (list (name d))))]
      [(hundreds h r)
       (append
         (list (name h) "hundred")
         (if (= r 0) empty (list "and" (name r))))]
      [1000 '("one thousand")])
    " "))
