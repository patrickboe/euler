(defn sundaram-to-prime [n] (+ 1 (* 2 n)))

(defn sundaram-row [n]
  #(let [row-inc (+ 1 (* 2 %))]
    (range
       (+ (* (- % 1) row-inc) (+ 1 (* % 3)))
       n
       row-inc)))

(defn sundaram-table [n]
  (remove empty?
    (map
      (sundaram-row n)
      (range 1 (+ 1 (quot n 3))))))

(defn decreasing? [x]
  (and (x 1) (> (first (x 0)) (first (x 1)))))

(defn take-bite [table]
  (let [[upwards downwards] (split-with decreasing? (partition 2 1 table))
       [unripe [[bite & bitten] & more]] (split-at (count upwards) table)]
    [bite (concat unripe (if bitten (cons bitten more) more))]))

(defn sundaram-sort [table]
  (lazy-seq
    (let [[k ks] (bite table)
          rest-sorted (sundaram-sort ks)]
      (or (and k (cons (to-composite k) rest-sorted)) rest-sorted))))

(defn remove-sundarams [sundarams xs]
  (lazy-seq
    (if (= (first xs) (first sundarams))
      (remove-sundarams (rest sundarams)(rest xs))
      (cons (first xs) (remove-sundarams (rest sundarams)(rest xs))))))

(defn primes-to [limit]
  (let [n (quot limit 2)
        numbers (int-array (range 0 n))]
    (cons 2
      (map sundaram-to-prime
        (remove-sundarams
          (sundaram-sort (sundaram-table n))
          numbers)))

(defn sum-primes-to [limit]
  (apply + (primes-to limit)))
