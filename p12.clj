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

(defn inc-exponents [[prev counts] x]
  (if
    (= prev x)
    [prev (cons (+ 1 (first counts)) (rest counts))]
    [x (cons 1 counts)]))

(defn exponents [xs]
  (second (reduce inc-exponents [0 nil] xs)))

(defn combination-count [items]
  (reduce * (map inc (exponents items))))

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
