(ns clj-ml.utils.linear-algebra-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clj-ml.utils.linear-algebra :refer [solve-quadratic-eq]]))

(deftest solve-quadratic-eq-test
  (testing "If the function can solve quadratic equations properly or not"
    (is (= (solve-quadratic-eq {:a 1 :b -3 :c 2}) [2.0 1.0]))
    (is (= (solve-quadratic-eq {:a 1 :b 7 :c 12}) [-3.0 -4.0]))
    (is (= (solve-quadratic-eq {:a 1 :b 3 :c -10}) [2.0 -5.0]))
    (is (= (solve-quadratic-eq {:a 1 :b -1 :c -30}) [6.0 -5.0]))))