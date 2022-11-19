(ns org.clojars.punit-naik.clj-ml.utils.linear-algebra-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.linear-algebra :as lau]))

(deftest eval-fn-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/eval-fn` correctly evaluates a function at a value or not"
    (is (= (lau/eval-fn [1 0 -5] 2) -1.0))
    (is (= (lau/eval-fn [1 0 -5] 1) -4.0))
    (is (= (lau/eval-fn [1 1 1] 2) 7.0))
    (is (= (lau/eval-fn [1 1 1] -1) 1.0))))

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
    (is (nil? (lau/find-all-possible-solutions [1 0 -5])))
    (is (= (lau/find-all-possible-solutions [1 0 -3 -2]) '(-2 -1 1 2)))
    (is (= (lau/find-all-possible-solutions [1 1 -11 -5 30]) '(-30 -15 -10 -6 -5 -3 -2 -1 1 2 3 5 6 10 15 30)))
    (is (= (lau/find-all-possible-solutions [3 -16 23 -6]) '(-6 -3 -2 -1 -2/3 -1/3 1/3 2/3 1 2 3 6)))))

(deftest isa-solution?-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/isa-solution?` checks if a number is a solution of the equation or not"
    (is (lau/isa-solution? [1 0 -5] 2.23606797749979))
    (is (lau/isa-solution? [1 0 -5] -2.23606797749979))
    (is (lau/isa-solution? [1 0 -3 -2] -1))
    (is (lau/isa-solution? [1 1 -11 -5 30] -3))))

(deftest solve-equation-synthetic-division-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/solve-equation-synthetic-division` correctly finds all the roots of the equation or not"
    (is (= (lau/solve-equation-synthetic-division [1 1 -11 -5 30]) [2.0 -3.0 2.23606 -2.23606]))
    (is (= (lau/solve-equation-synthetic-division [1 0 -3 -2]) '(2.0 -1.0 -1.0)))))

(deftest newtons-method-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.linear-algebra/newtons-method` function"
    (is (= (lau/newtons-method [1 0 -5] [2 0] 5 -2.23606) -2.23606797749979))
    (is (= (lau/newtons-method [1 0 -5] [2 0] 5 2.23606) 2.23606797749979))))

(deftest solve-equation-newtons-method-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/solve-equation-newtons-method` correctly finds all the roots of the equation or not"
    (is (= (lau/solve-equation-newtons-method [1 1 -11 -5 30]) '(-3.0 -2.23606 1.99999 2.0 2.23606)))
    (is (= (lau/solve-equation-newtons-method [1 0 -3 -2]) '(-1.0 2.0)))
    (is (= (lau/solve-equation-newtons-method [-1 5 3 -6]) '(-1.24889 0.8978 5.35109)))))

(deftest solve-equation-test
  (testing "if the function `org.clojars.punit-naik.clj-ml.utils.linear-algebra/solve-equation` correctly finds all the roots of the equation or not"
    (is (= (lau/solve-equation [1 1]) [-1.0]))
    (is (= (lau/solve-equation [1 -1]) [1.0]))
    (is (= (lau/solve-equation [2 1]) [-0.5]))
    (is (= (lau/solve-equation [2 -1]) [0.5]))
    (is (= (lau/solve-equation [1 -3 2]) [2.0 1.0]))
    (is (= (lau/solve-equation [1 7 12]) [-3.0 -4.0]))
    (is (= (lau/solve-equation [1 3 -10]) [2.0 -5.0]))
    (is (= (lau/solve-equation [1 -1 -30]) [6.0 -5.0]))
    (is (= (lau/solve-equation [1 0 -5]) [2.23606 -2.23606]))
    (is (= (lau/solve-equation [1 1 -11 -5 30]) [2.0 -3.0 2.23606 -2.23606]))
    (is (= (lau/solve-equation [1 0 -3 -2]) '(2.0 -1.0 -1.0)))
    (is (= (lau/solve-equation [-1 5 3 -6]) '(-1.24889 0.8978 5.35109)))
    (is (= (lau/solve-equation [2 -5 -14 8]) [4.0 0.5 -2.0]))
    (is (= (lau/solve-equation [3 -16 23 -6]) [3.0 2.0 0.33333]))
    (is (every? #(Double/isNaN %) (lau/solve-equation [1 4 6])))))
