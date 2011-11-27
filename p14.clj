(defn greater-than-one? [x] (> x 1))

(defn next-term [k]
  (if (greater-than-one? k)
    (if
      (even? k)
      (/ k 2)
      (+ 1 (* 3 k)))))

(defn seq-from [n]
  (take-while identity (iterate next-term n)))

(defn count-from [n]
  (if n
    (+ 1 (count-from (next-term n)))
    0))

(def count-from (memoize count-from))

(defn max-count [a b]
  (if (> (count-from a) (count-from b)) a b))

(defn max-seq-under [n]
  (reduce max-count 1 (take-while #(< % n) (range 2 n))))
