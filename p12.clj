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

(defn inc-counts [[prev counts] x]
  (if
    (= prev x)
    [prev (cons (+ 1 (first counts)) (rest counts))]
    [x (cons 1 counts)]))

(defn combination-count [items]
  (reduce inc-counts [0 nil] items))

(defn factor-count [n]
  (+ 1 (combination-count (prime-factors n))))

(defn next-triangle [prev n]
  [(+ n prev) (+ 1 n)])

(defn triangles []
  (map first (iterate #(apply next-triangle %) [1 2])))

(defn n-factored-triangle [n]
  (let [n! (reduce * (range 1 n))]
    (first (filter #(> (factor-count %) n) (drop-while #(< % n!) (triangles))))))
