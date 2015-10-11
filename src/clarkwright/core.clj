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


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
