(ns clarkwright.core)



(defn length
  [path costs]
  (letfn [(get-costs [& xs]
            (get-in costs xs))]
    (apply + (map get-costs path (rest path)))))



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
