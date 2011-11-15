(defn divides? [x y]
  (= 0 (mod x y)))

(defn palindrome? [n]
  (let [s (Integer/toString n)]
    (= (take 3 s) (reverse (drop 3 s)))))

(defn palindromes []
  (filter palindrome? (range 998000 100001 -1)))

(defn between [x a b]
  (and (> x a) (< x b)))

(defn three-digit-quotient? [x y]
  (and (divides? x y) (between (/ x y) 99 1000)))

(defn has-three-digit-factors [x]
  (some (partial three-digit-quotient? x) (range 999 100 -1)))

(defn palindromes-with-three-digit-factors []
  (filter has-three-digit-factors (palindromes)))
