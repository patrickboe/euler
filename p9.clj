(defn square [x] (* x x))

(defn py-triplets []
  (filter nil? (map get-triplet (map square (range 1)))))

(defn divides? [a b]
  (= 0 (mod a b)))

(defn whole? [x]
  (divides? x 1))

(defn is-square [x]
  (whole? (sqrt x)))

(defn are-triplets [a b]
  (is-square (square a) (square b)))

(defn get-triplet [a]
  (let [b] (first (filter (partial are-triplets a) (range a (* 2 a)))))


