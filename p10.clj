(use '[clojure.string :only [join]])

(defn nonsundaram-to-prime [n] (+ 1 (* 2 n)))

(defn dump [table]
  (println (join \newline (map #(join " " (map str %)) table))))

(defn sundaram-row [end step]
  (let [row-inc (+ 1 (* 2 step))]
    (range
       (+ (* (- step 1) row-inc) (+ 1 (* step 3)))
       end
       row-inc)))

(defn sundaram-table [n]
  (remove empty?
    (map
      (partial sundaram-row n)
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

(defn flat-sundaram
  ([table] (flat-sundaram nil table))
  ([prev-bite table]
    (lazy-seq
      (if (not-empty table)
        (let [[bite ks] (take-bite table)]
          (if (= bite prev-bite)
            (flat-sundaram prev-bite ks)
            (cons bite (flat-sundaram bite ks))))
        nil))))

(defn remove-sundarams [sundarams xs]
  (lazy-seq
    (if (not-empty xs)
      (if (= (first xs) (first sundarams))
        (remove-sundarams (rest sundarams) (rest xs))
        (cons (first xs) (remove-sundarams sundarams (rest xs))))
      nil)))

(defn primes-to [limit]
  (let [n (quot limit 2)
        numbers (int-array (range 1 n))]
    (cons 2
      (map nonsundaram-to-prime
        (remove-sundarams
          (flat-sundaram (sundaram-table n))
          numbers)))))

(defn sum-primes-to [limit]
  (apply + (primes-to limit)))
