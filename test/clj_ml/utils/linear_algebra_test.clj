(ns clj-ml.utils.linear-algebra-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-ml.utils.linear-algebra :as lau]))

(deftest solve-quadratic-eq-test
  (testing "If the function `clj-ml.utils.linear-algebra/solve-quadratic-eq` can solve quadratic equations properly or not"
    (is (= (lau/solve-quadratic-eq {:a 1 :b -3 :c 2}) [2.0 1.0]))
    (is (= (lau/solve-quadratic-eq {:a 1 :b 7 :c 12}) [-3.0 -4.0]))
    (is (= (lau/solve-quadratic-eq {:a 1 :b 3 :c -10}) [2.0 -5.0]))
    (is (= (lau/solve-quadratic-eq {:a 1 :b -1 :c -30}) [6.0 -5.0]))))

(deftest factors-test
  (testing "if the function can `clj-ml.utils.linear-algebra/factors` function can correctly find the factors of"
    (testing "integers"
      (is (= (lau/factors 253) #{1 23 11 253}))
      (is (= (lau/factors 254) #{1 2 127 254}))
      (is (= (lau/factors 100) #{20 1 4 50 100 25 2 5 10})))
    (testing "floats"
      (is (= (lau/factors 2.53 true) #{1}))
      (is (= (lau/factors 2.54 true) #{1 2})))))