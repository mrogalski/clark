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

(def remove-losses
  (filter (comp pos? last)))

(defn calculate-savings-for-all-points
  [graph depot-key]
  (let [points                 (keys (depot-key graph))
        point-pairs->savings   (map (partial calculate-savings graph depot-key))
        point-pairs-with-loops (cartesian-product points points)]
    (sequence
     (comp delete-loops point-pairs->savings remove-losses)
     point-pairs-with-loops)))


(defn find-endpoints-in-paths
  [paths endpoints]
  (let [has-endpoints? (partial some (set endpoints))
        groupped (group-by has-endpoints? paths)
        start-path (first (groupped (first endpoints)))
        end-path (first (groupped (second endpoints)))
        all-others (groupped nil)]
    [start-path end-path all-others]))


(defn strip-endpoints [x]
  (-> x
      rest
      butlast))

(defn connect [x y]
  (concat (butlast x) (rest y)))

(defn get-first-and-last [path]
  ((juxt first last)
   (strip-endpoints path)))

(defn connect-if-possible [pathx pathy endpoints]
  (let [[startx endx] (get-first-and-last pathx)
        [starty endy] (get-first-and-last pathy)]
    (condp = endpoints
      [endx starty] [(connect pathx pathy)]
      [endy startx] [(connect pathy pathx)]
      (remove nil? [pathx pathy]))))


(defn quantity-cost
  [quantities path]
  (->> path
       strip-endpoints
       (select-keys quantities)
       vals
       (apply +)))

(defn exceeds-capacity?
  [capacity quantities path]
  (< capacity (quantity-cost quantities path)))


(defn clarkewright
  [graph depot-key quantities total-car-capacity]
  (let [paths          (generate-initial-paths graph depot-key (dec (count graph)))
        savings        (calculate-savings-for-all-points graph depot-key)
        sorted-savings (sort-by last > savings)]
    (loop [s sorted-savings
           ps paths]
      (if (empty? s)
        ps
        (let [endpoints              (butlast (first s))
              [start-path
               end-path
               all-others]           (find-endpoints-in-paths ps endpoints)
              maybe-connected        (connect-if-possible start-path
                                                          end-path
                                                          endpoints)
              connection-made?       (= 1 (count maybe-connected))
              path-exceeds-capacity? (exceeds-capacity?
                                      total-car-capacity
                                      quantities
                                      (first-maybe-connected))
              new-paths              (if (and connection-made?
                                              path-exceeds-capacity?)
                                       ps
                                       (concat all-others maybe-connected))]
          (recur (rest s) new-paths))))))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
