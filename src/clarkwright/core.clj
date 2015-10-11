(ns clarkwright.core)


(defn path+costs->edge-costs
  [path costs]
  ((apply juxt (map comp path (rest path))) costs))

(defn length
  [path costs]
  (apply + (path+costs->edge-costs path costs)))



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
