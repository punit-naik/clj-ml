(ns org.clojars.punit-naik.clj-ml.utils.linear-algebra-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.linear-algebra :as lau]))

(deftest solve-quadratic-equation-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/solve-quadratic-equation` can solve quadratic equations properly or not"
    (is (= (lau/solve-quadratic-equation {:a 1 :b -3 :c 2}) [2.0 1.0]))
    (is (= (lau/solve-quadratic-equation {:a 1 :b 7 :c 12}) [-3.0 -4.0]))
    (is (= (lau/solve-quadratic-equation {:a 1 :b 3 :c -10}) [2.0 -5.0]))
    (is (= (lau/solve-quadratic-equation {:a 1 :b -1 :c -30}) [6.0 -5.0]))))

(deftest factors-test
  (testing "if the function can `org.clojars.punit-naik.clj-ml.utils.linear-algebra/factors` function can correctly find the factors of"
    (testing "integers"
      (is (= (lau/factors 253) #{1 23 11 253}))
      (is (= (lau/factors 254) #{1 2 127 254}))
      (is (= (lau/factors 100) #{20 1 4 50 100 25 2 5 10})))
    (testing "floats"
      (is (= (lau/factors 2.53 true) #{1}))
      (is (= (lau/factors 2.54 true) #{1 2})))))

(deftest find-all-possible-solutions-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/find-all-possible-solutions` finds all possible solutions correctly"
    (is (= (lau/find-all-possible-solutions [1 0 -5]) [2.23606797749979 -2.23606797749979]))
    (is (= (lau/find-all-possible-solutions [1 0 -3 -2]) '(-2 -1 1 2)))
    (is (= (lau/find-all-possible-solutions [1 1 -11 -5 30]) '(-30 -15 -10 -6 -5 -3 -2 -1 1 2 3 5 6 10 15 30)))))

(deftest isa-solution?-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/isa-solution?` checks if a number is a solution of the euqation or not"
    (is (lau/isa-solution? [1 0 -5] 2.23606797749979))
    (is (lau/isa-solution? [1 0 -5] -2.23606797749979))
    (is (lau/isa-solution? [1 0 -3 -2] -1))
    (is (lau/isa-solution? [1 1 -11 -5 30] -3))))

(deftest solve-equation-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/solve-equation` correctly finds all the roots of the euqation or not"
    (is (= (lau/solve-equation [1 0 -5]) '(-2.23606797749979 2.23606797749979)))
    (is (= (lau/solve-equation [1 1 -11 -5 30]) '(2.0 -3.0 -2.23606797749979 2.23606797749979)))
    (is (= (lau/solve-equation [1 0 -3 -2]) '(2.0 -1.0 -1.0)))))