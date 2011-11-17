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

(defn low-row-count
  ([table] (low-row-count table 0))
  ([table tally]
    (let [new-tally (+ 1 tally)]
      (if (= 0 (first (first table)))
        new-tally
        (recur (rest table) new-tally)))))

(defn split-by-consumable [table]
  (let [[consuming dormant] (split-at (low-row-count table) table)]
    [(drop-while empty? consuming) dormant]))

(defn firsts [table]
  (map first table))

(defn prune-row [value]
  #(if (= value (first %)) (rest %) %))

(defn prune [table value]
  (map (prune-row value) table))

(defn sundaram-series [table]
  (lazy-seq
    (let [[consumable dormant] (split-by-consumable table)]
      (and
        consumable
        (let [lowest-n (apply min (firsts consumable))
              rest-of-table (concat (prune consumable lowest-n) dormant)]
          (cons lowest-n (sundaram-series rest-of-table)))))))
