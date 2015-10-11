(ns clarkwright.core-test
  (:require [clojure.test :refer :all]
            [clarkwright.core :refer :all]))

(def costs
  {:depot {:a 5
           :b 7}
   :a {:depot 5
       :b 3}
   :b {:a 3
       :depot 7}})

(deftest length-test
  (testing "path length testing"
    (is (= 3 (length [:a :b] costs)))
    (is (= 10 (length [:depot :a :depot] costs)))
    (is (= 14 (length [:depot :b :depot] costs)))
    (is (= 15 (length [:depot :a :b :depot] costs)))))

(deftest initial-paths-test
  (testing "generation of initial paths"
    (is (= '([:depot :a :depot] [:depot :b :depot])
           (generate-initial-paths costs :depot 2)))
    (is (= `([:depot ~(ffirst (:depot costs)) :depot])
           (generate-initial-paths costs :depot 1)))))
