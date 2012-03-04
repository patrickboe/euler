#lang lazy

(define (lex-permutations lst)
  (reverse-lex-permutations (reverse lst)))

(define (reverse-lex-permutations lst)
  (if (empty? (cdr lst))
    (list lst)
    (foldl append '()
      (map (λ(a)
        (map (λ(l) (cons a l))
          (reverse-lex-permutations (remove a lst))))
      lst))))

(display (list-ref (lex-permutations '(0 1 2 3 4 5 6 7 8 9)) 999999))
