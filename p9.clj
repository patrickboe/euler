(use 'clojure.contrib.math)

(defn square [x] (* x x))

(defn sum-of-squares [& operands]
  (reduce + (map square operands)))

(defn make-goal-triplet [goal-sum a b]
  (let [c (sqrt (sum-of-squares a b))]
    (if (= (+ a b c) goal-sum)
      [a b c])))

(defn triplet-for [goal-sum a]
   (some
     (partial make-goal-triplet goal-sum a)
     (range a (- goal-sum (* 2 a)))))

(defn first-goal-triplet [goal-sum]
  (some
    (partial triplet-for goal-sum)
    (range 1 goal-sum)))

(apply * (first-goal-triplet 1000))
