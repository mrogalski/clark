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

(def costs2
  {:depot {:a 5
           :b 7
           :c 9}
   :a {:depot 5
       :b 3
       :c 8}
   :b {:a 3
       :depot 7
       :c 5}
   :c {:a 8
       :depot 9
       :b 5}})

(deftest length-test
  (testing "path length testing"
    (is (= 3 (length [:a :b] costs)))
    (is (= 10 (length [:depot :a :depot] costs)))
    (is (= 14 (length [:depot :b :depot] costs)))
    (is (= 15 (length [:depot :a :b :depot] costs)))))

(deftest initial-paths-test
  (testing "generation of initial paths"
    (is (= #{[:depot :a :depot] [:depot :b :depot]}
           (into #{} (generate-initial-paths costs :depot 2))))
    (is (= `([:depot ~(ffirst (:depot costs)) :depot])
           (generate-initial-paths costs :depot 1)))
    (is (= #{[:depot :c :depot] [:depot :a :depot] [:depot :b :depot]}
           (into #{} (generate-initial-paths costs2 :depot 3))))
    ))

(deftest costs-test
  (testing "cost tests"
    (is (= #{[:b :a 9] [:a :b 9]}
           (into #{} (calculate-savings-for-all-points costs :depot))))
    (is (= #{[:a :b 9] [:a :c 6] [:b :a 9] [:b :c 11] [:c :a 6] [:c :b 11]}
           (into #{} (calculate-savings-for-all-points costs2 :depot))))))
