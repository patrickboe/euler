(use 'factors)

(defn square [x] (* x x))

(defn square-equals [a b]
  (if (= a b) (square a) a))

(defn cons-if [pred x xs]
  (if pred (cons x xs) xs))

(defn prime-factors [n]
  (lazy-seq
    (let
      [first-factor
         (first
          (filter #(= 0 (mod n %))
            (take-while #(<= (square %) n) (primes))))]
      (or
        (and
          first-factor
          (cons
            first-factor
            (prime-factors (/ n first-factor))))
        (list n)))))

(def prime-factors (memoize prime-factors))

(defn inc-dupes [[prev dupe-counts unique] x]
  (if
    (= prev x)
    [prev (cons (+ 1 (first dupe-counts)) (rest dupe-counts)) unique]
    [x (cons 0 dupe-counts) (+ 1 unique)]))

(defn dup-coeff [x]
  (+ 1 (/ x 2)))

(defn to-power [x y]
  (reduce * (repeat y x)))

(defn nonzero [x]
  (not (= 0 x)))

(defn combination-count [items]
  (let [[_ dupe-counts unique] (reduce inc-dupes [0 nil 0] items)]
    (*
      (to-power 2 unique)
      (reduce * 1 (map dup-coeff (filter nonzero dupe-counts))))))

(defn factor-count [n]
  (combination-count (prime-factors n)))

(defn combinations [items]
  (and
    (not-empty items)
    (let [c (combinations (rest items))]
      (cons
        (take 1 items)
        (concat  (map #(cons (first items) %) c) c)))))

(defn factors [n]
  (cons 1 (sort (distinct (map #(reduce * %) (combinations (prime-factors n)))))))

(defn alt-factor-count [n]
  (count (factors n)))

(defn fc-compare []
  (some #(if (= (factor-count %) (alt-factor-count %)) false %) (drop 2 (range))))

(defn next-triangle [prev n]
  [(+ n prev) (+ 1 n)])

(defn triangles []
  (map first (iterate #(apply next-triangle %) [1 2])))

(defn n-factored-triangle [n]
  (let [n! (reduce * (range 1 n))]
    (first (filter #(> (factor-count %) n) (triangles)))))
