(use 'factors)

(defn square [x] (* x x))

(defn square-equals [a b]
  (if (= a b) (square a) a))

(defn cons-if [pred x xs]
  (if pred (cons x xs) xs))

(defn factors-except [ignore n]
  (lazy-seq
    (cons-if (not (= n ignore)) n
      (let 
        [first-factor
           (first
            (filter #(= 0 (mod n %))
              (take-while #(<= (square %) n) (primes))))]
        (and
          first-factor
          (let [yielded (square-equals first-factor ignore)]
            (cons
              yielded
              (factors-except yielded (/ n first-factor)))))))))

(defn factors-of [n]
  (cons 1 (factors-except 1 n)))

(defn next-triangle [prev n]
  [(+ n prev) (+ 1 n)])

(defn triangles []
  (map first (iterate #(apply next-triangle %) [1 2])))
