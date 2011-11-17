(use 'clojure.contrib.math)

(defn sundaram-row [t-limit]
  #(cons 0 (range
     (+ 1 (* % 3))
     t-limit
     (+ 1 (* 2 %)))))

(defn sundaram-table [limit]
  (let [t-limit (quot limit 2)]
    (map
      (sundaram-row t-limit)
      (range 1 (- (quot t-limit 3) 1)))))

(defn low-rows [table]
  (lazy-seq
    (let [r (first table)
          n (first r)]
      (if (= 0 n)
        (list (rest r))
        (cons r (low-rows (rest table)))))))

(defn firsts [table]
  (map first table))

(defn lowest-n [table]
  (apply min (firsts (low-rows table))))

(defn next-sundaram [table]
  (let [n (lowest-n table)]))
