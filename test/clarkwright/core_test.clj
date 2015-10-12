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

(def graph-cw
  {:1  {:2  25
        :3  43
        :4  57
        :5  43
        :6  61
        :7  29
        :8  41
        :9  48
        :10 71}

   :2  {:1  25
        :3  29
        :4  34
        :5  43
        :6  68
        :7  49
        :8  66
        :9  72
        :10 71}

   :3  {:1  43
        :2  29
        :4  52
        :5  72
        :6  96
        :7  72
        :8  81
        :9  89
        :10 114}

   :4  {:1  57
        :2  34
        :3  52
        :5  45
        :6  71
        :7  71
        :8  95
        :9  99
        :10 108}

   :5  {:1  43
        :2  43
        :3  72
        :4  45
        :6  27
        :7  36
        :8  65
        :9  65
        :10 65}

   :6  {:1  61
        :2  68
        :3  96
        :4  71
        :5  27
        :7  40
        :8  66
        :9  62
        :10 46}

   :7  {:1  29
        :2  49
        :3  72
        :4  71
        :5  36
        :6  40
        :8  31
        :9  31
        :10 43}


   :8  {:1  41
        :2  66
        :3  81
        :4  95
        :5  65
        :6  66
        :7  31
        :9  11
        :10 46}

   :9  {:1  48
        :2  72
        :3  89
        :4  99
        :5  65
        :6  62
        :7  31
        :8  11
        :10 36}

   :10 {:1 71
        :2 91
        :3 114
        :4 108
        :5 65
        :6 46
        :7 43
        :8 46
        :9 36}})

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
