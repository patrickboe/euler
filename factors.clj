(ns factors)
(use 'clojure.contrib.math)

(defn divides? [x y]
  (= 0 (mod x y)))

(defn plus2 [x] (+ x 2))

(defn eliminate-factor [x factor]
  (let [d (/ x factor)]
    (if (not (divides? d factor))
      d
      (recur d factor))))

(defn limited-factors-in [x coll lim]
  (lazy-seq 
    (let [guess (first coll)]
      (if (divides? x guess)
        (cons guess (limited-factors-in x (rest coll) x))
        (if (or (> guess lim) (empty? (rest coll)))
          nil
          (limited-factors-in x (rest coll) lim))))))

(defn factors-in [x coll]
  (limited-factors-in x coll (sqrt x)))

(defn none-are-factors? [coll]
  #(empty? (factors-in % coll)))
  
(defn next-prime [last-known known]
 (first
   (filter
     (none-are-factors? known)
     (iterate plus2 (plus2 last-known)))))

(defn add-next-prime [last-known known]
  (let [n (next-prime last-known known)]
    [n (conj known n)]))

(defn keyed-memoize [f args-to-key]
  (let [mem (atom {})]
    (fn [& args]
      (let [mkey (args-to-key args)]
        (if-let [e (find @mem mkey)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc mkey ret)
            ret))))))

(def add-next-prime (keyed-memoize add-next-prime first))

(defn primes []
  (cons 2 (map first (iterate #(apply add-next-prime %) [3 [2 3]]))))

(defn sum-primes-under [x]
  (reduce + (take-while #(< % x) (primes))))
