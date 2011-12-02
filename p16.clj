(defn pwr2-digit-sum [n]
  (apply + (map #(. Integer parseInt (str %)) (str (. (. BigInteger ONE) shiftLeft n)))))

(pwr2-digit-sum 1000)
