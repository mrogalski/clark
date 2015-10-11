(ns clarkwright.core)


(defn path+costs->edge-costs
  [path costs]
  ((apply juxt (map comp path (rest path))) costs))

(defn length
  [path costs]
  (apply + (path+costs->edge-costs path costs)))

(defn generate-initial-paths
  [graph depot-key number-of-vehicles]
  (letfn [(point->path-from-depot-and-back [point]
            [depot-key point depot-key])]
    (->> (graph depot-key)
         keys
         (map point->path-from-depot-and-back)
         (take number-of-vehicles))))

(defn calculate-savings
  [graph depot-key [point-key1 point-key2]]
  (let [from-depot     (graph depot-key)
        depot->point1  (from-depot point-key1)
        depot->point2  (from-depot point-key2)
        point1->point2 (get-in graph [point-key1 point-key2])]
    {point-key1
     {point-key2
      (+ depot->point1 depot->point2 (- point1->point2))}}))

(defn cartesian-product
  [x y]
  (mapcat #(map (partial vector %) y) x))

(def delete-loops
  (remove (partial apply =)))

(defn calculate-savings-for-all-points
  [graph depot-key]
  (let [points                           (keys (depot-key graph))
        calculate-savings-for-point-pair (map (partial calculate-savings graph depot-key))
        deep-merge                       (partial merge-with merge)]
    (->> (cartesian-product points points)
         (sequence (comp delete-loops calculate-savings-for-point-pair))
         (reduce deep-merge))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
