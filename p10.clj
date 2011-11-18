(use 'clojure.contrib.math)

(defn composite-relative [n] (+ 1 (* 2 n)))

(defn sundaram-row [t-limit]
  #(map composite-relative (range
     (+ 1 (* % 3))
     t-limit
     (+ 1 (* 2 %)))))

(defn sundaram-table [limit]
  (let [t-limit (quot limit 2)]
    (remove empty?
      (map
        (sundaram-row t-limit)
        (range 1 (+ 1 (quot t-limit 3)))))))
