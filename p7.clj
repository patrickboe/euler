(use 'clojure.contrib.math)

(defn divides? [x y]
  (= 0 (mod x y)))

(defn plus2 [x] (+ x 2))

(defn eliminate-factor [x factor]
  (let [d (/ x factor)]
    (if (not (divides? d factor))
      d
      (eliminate-factor d factor))))

(defn limited-factors-in [x coll lim]
  (lazy-seq 
    (let [guess (first coll)]
      (if (divides? x guess)
        (let [defactored (eliminate-factor x guess)]
          (cons
            guess
            (limited-factors-in defactored (rest coll) defactored)))
        (if (or (> guess lim) (empty? (rest coll)))
          nil
          (limited-factors-in x (rest coll) lim))))))

(defn factors-in [x coll]
  (limited-factors-in x coll (sqrt x)))

(defn next-prime [known]
  (let [rknown (reverse known)]
    (first
      (filter
        #(empty? (factors-in % rknown))
        (iterate plus2 (plus2 (first known)))))))

(defn primes
  ([]
    (concat (list 2 3) (primes (list 3 2))))
  ([known]
    (lazy-seq
      (let [m (next-prime known)]
        (cons m (primes (cons m known)))))))
