(use 'factors)

(defn square [x] (* x x))

(defn square-equals [a b]
  (if (= a b) (square a) a))

(defn cons-if [pred x xs]
  (if pred (cons x xs) xs))

(defn lazy-factors [n]
  (lazy-seq
    (cons n
      (let 
        [first-factor
           (first
            (filter #(= 0 (mod n %))
              (take-while #(<= (square %) n) (primes))))]
        (and
          first-factor
          (cons
              first-factor
              (lazy-factors (/ n first-factor))))))))

(def lazy-factors (memoize lazy-factors))

(defn factors-of [n]
  (distinct (cons 1 (lazy-factors n))))

(defn next-triangle [prev n]
  [(+ n prev) (+ 1 n)])

(defn triangles []
  (map first (iterate #(apply next-triangle %) [1 2])))

(defn n-factored-triangle [n]
  (first (filter #(> (count (factors-of %)) n) (triangles))))
