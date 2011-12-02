(defn paths
  ([m n]
   (if (>= n m)
     (if
       (= 1 m)
       1
       (+
         (paths (- m 1) n)
         (paths m (- n 1))))
     (paths n m))))

(def paths (memoize paths))

(defn square-paths [edge-width]
  (let [node-width (+ 1 edge-width)]
    (paths node-width node-width)))

(square-paths 20)
