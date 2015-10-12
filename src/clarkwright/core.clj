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


(defn all-empty? [& xs]
  (->> xs
       (map empty?)
       (some true?)))


(defn strip-endpoints [x]
  (-> x
      rest
      butlast))

(defn connect [x y]
  (concat (butlast x) (rest y)))

(defn connect-if-possible [x y [pathx pathy]]
  (println x y pathx pathy (strip-endpoints x) (strip-endpoints y))
  (let [[sx ex] ((juxt first last) (strip-endpoints x))
        [sy ey] ((juxt first last) (strip-endpoints y))]
    (cond
      (and (= ex pathx) (= sy pathy)) [(connect x y)]
      (and (= ey pathx) (= sx pathy)) [(connect y x)]
      :else [x y])))


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
              [start-path end-path all-others] (find-endpoints-in-paths ps endpoints)
              endpoints-one-one-path? (all-empty? start-path end-path)]
          (if endpoints-one-one-path?
            (recur (rest s) ps)
            (recur (rest s) (concat all-others (connect-if-possible start-path end-path endpoints)))))
      )
    )

  ))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
