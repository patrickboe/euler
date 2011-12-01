(:use 'clojure.contrib.graph)

(defn square [x] (* x x))

(defn grid [n]
  (let [r (range 0 (+ 1 n))]
    (map (fn [i] (map (fn [j] (list i j)) r)) r)))

(defn potential-neighbors [node]
  (list
    (list (+ (first node) 1) (second node))
    (list (- (first node) 1) (second node))
    (list (first node) (+ (second node) 1))
    (list (first node) (- (second node) 1))))

(defn legal-for [n node]
  (and
    (>= (first node) 0)
    (>= (second node) 0)
    (>= n (first node))
    (>= n (second node))))

(defn neighbor-select [n edges]
  (fn [node]
    (filter (partial legal-for n) 
      (potential-neighbors node))))

(defn adjacency-graph
  ([n]
   (adjacency-graph
     n
     (apply vector-of :boolean (repeat (square (+ 1 n)) true))))
  ([n edges]
   (struct directed-graph
     (grid n)
     (neighbor-select n edges))))
