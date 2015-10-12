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
        point1->point2 (get-in graph [point-key1 point-key2])
        savings        (+ depot->point1
                          depot->point2
                          (- point1->point2))]
    [point-key1 point-key2 savings]))

(defn cartesian-product
  [x y]
  (mapcat #(map (partial vector %) y) x))

(def delete-loops
  (remove (partial apply =)))

(defn calculate-savings-for-all-points
  [graph depot-key]
  (let [points                 (keys (depot-key graph))
        point-pairs->savings   (map (partial calculate-savings graph depot-key))
        point-pairs-with-loops (cartesian-product points points)]
    (sequence
     (comp delete-loops point-pairs->savings)
     point-pairs-with-loops)))

(defn third [xs]
  (nth xs 2))


(defn find-endpoints-in-paths
  [paths endpoints]
  (let [has-endpoints? (partial some (set endpoints))
        groupped (group-by has-endpoints? paths)
        start-path (first (groupped (first endpoints)))
        end-path (first (groupped (second endpoints)))
        all-others (groupped nil)]
    [start-path end-path all-others]))


(defn clarkewright
  [graph depot-key]
  (let [paths          (generate-initial-paths graph depot-key (dec (count graph)))
        savings        (calculate-savings-for-all-points graph depot-key)
        sorted-savings (sort-by third > savings)]
    (loop [s sorted-savings
           ps paths]
      (if (empty? s)
        ps
        (let [endpoints (butlast (first s))
              [start-path end-path all-others] (find-endpoints-in-paths ps endpoints)]
          (if (or (empty? start-path)
                  (empty? end-path))
            (recur (rest s) ps)
            (do
              (println start-path end-path all-others)
              (recur (rest s) ps))))
      )
    )

  ))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
