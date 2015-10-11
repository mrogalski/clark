(ns clarkwright.core)

(def costs
  {:depot {:a 5
           :b 7}
   :a {:depot 5
       :b 3}
   :b {:a 3
       :depot 7}})


(defn length
  [path costs]
  (letfn [(get-costs [& xs]
            (get-in costs xs))]
    (apply + (map get-costs path (rest path)))))



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
