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

(defn primes-to [limit]
  (let [n (quot limit 2)
        numbers (int-array (range 0 n)) 
        sundaram-numbers (apply concat (sundaram-table n))]
    (dorun (map #(aset numbers % 0) sundaram-numbers))
    (cons 2 (map sundaram-to-prime (remove zero? (seq numbers))))))

(defn sum-primes-to [limit]
  (apply + (primes-to limit)))
