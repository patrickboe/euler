(defn non-one? [x] (not (= x 1)))

(defn next-term [k]
  (if (non-one? k)
    (if
      (even? k)
      (/ k 2)
      (+ 1 (* 3 k)))))

(defn count-from [n]
  (if n
    (+ 1 (count-from (next-term n)))
    0))

(def count-from (memoize count-from))

(defn best-iter [beats? value-of]
  (fn [[score _ :as running] k]
    (let [v (value-of k)]
      (if
        (beats? score v)
        running
        [v k]))))

(defn best [beats? value-of [i & r]]
  (second (reduce
    (best-iter beats? value-of)
    [(value-of i) i]
    r)))

(defn max-seq-under [n]
  (best > count-from (range 1 n)))
