(use '[clojure.string :only [join]])

(defn nonsundaram-to-prime [n] (+ 1 (* 2 n)))

(defn sump [table]
  (println (join \newline (map #(join " " (map str %)) table))))

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
  (and
    (not-empty (last x))
    (> (first (first x)) (first (last x)))))

(defn by-twos [table]
  (partition 2 1 (list nil) table))

(defn take-bite [table]
  (let [[upwards downwards] (split-with decreasing? (by-twos table))
       [unripe [[bite & bitten] & more]] (split-at (count upwards) table)]
    [bite (concat unripe (if bitten (cons bitten more) more))]))

(defn sundaram-sort
  ([table] (sundaram-sort nil table))
  ([prev-bite table]
    (lazy-seq
      (if (not-empty table)
        (let [[bite ks] (take-bite table)]
          (if (= bite prev-bite)
            (sundaram-sort prev-bite ks)
            (cons bite (sundaram-sort bite ks))))
        nil))))

(defn remove-sundarams [sundarams xs]
  (lazy-seq
    (let [remaining (remove-sundarams (rest sundarams)(rest xs))]
    (if (= (first xs) (first sundarams))
      remaining
      (cons (first xs) remaining)))))

(defn primes-to [limit]
  (let [n (quot limit 2)
        numbers (int-array (range 0 n))]
    (cons 2
      (map nonsundaram-to-prime
        (remove-sundarams
          (sundaram-sort (sundaram-table n))
          numbers)))))

(defn sum-primes-to [limit]
  (apply + (primes-to limit)))
