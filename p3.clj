(defn divides? [x y]
  (= 0 (mod x y)))

(defn plus2 [x] (+ x 2))

(defn eliminate-factor [x factor]
  (let [d (/ x factor)]
    (if (not (divides? d factor))
      d
      (eliminate-factor d factor))))

(defn factors-in [x coll]
  (lazy-seq 
    (let [guess (first coll)]
      (if (divides? x guess)
        (cons
          guess (factors-in (eliminate-factor x guess) (rest coll)))
        (if (or (> guess x) (empty? (rest coll)))
          nil
          (factors-in x (rest coll)))))))

(defn next-prime [known]
  (first
    (filter
      #(empty? (factors-in % (reverse known)))
      (iterate plus2 (plus2 (first known))))))

(defn prime-iter [known-primes] 
  (let [n (next-prime known-primes)]
    (cons n known-primes)))

(defn primes []
  (map first (cons [2] (iterate prime-iter [3 2]))))
