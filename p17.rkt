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
      (90 . "ninety")
      (100 . "hundred")
      (1000 . "thousand")))

(define (range-list a b) (stream->list (in-range a b)))

(define digits (range-list 1 10))

(define magnitudes '(100 1000))

(define teens (range-list 10 20))

(define decades (map (lambda (x) (* 10 x)) (rest digits)))

(define (subcential? x) (< x 100))

(define (subvential? x) (< x 20))

(define (name-subvential x) (hash-ref num-names x))

(define (name-subcential x)
  (if (subvential? x)
    (name-subvential x)
    (let*
      ([d (modulo x 10)]
       [t (hash-ref num-names (- x d))])
      (if (= 0 d)
        t
        (string-append t " " (name-subvential d))))))
