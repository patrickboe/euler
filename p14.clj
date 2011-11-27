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

(defn max-count [[run-max run-best :as running] k]
  (let [c (count-from k)]
    (if
      (> run-max c)
      running
      [c k])))

(defn max-seq-under [n]
  (second
    (reduce
      max-count
      [1 1]
      (range 2 n))))
