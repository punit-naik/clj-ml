(ns org.clojars.punit-naik.clj-ml.utils.calculus-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.calculus :as cu]))

(defonce ^:private derivative-test-input-1 [1 0 -5]) ; => x^2-5
(defonce ^:private derivative-test-output-1 [2 0]) ; => 2x
(defonce ^:private derivative-test-input-2 [1 3 2 4]) ; => x^3+3x^2+2x+4
(defonce ^:private derivative-test-output-2 [3 6 2]) ; => 3x^2+6x+2
(defonce ^:private derivative-test-input-3 [4 3 2 1 10]) ; => 4x^4+3x^3+2x^2+x+10
(defonce ^:private derivative-test-output-3 '(16 9 4 1)) ; => 16x^3+9x^2+4x+1

(deftest derivative-test
  (testing "If the function correctly finds the derivative of a function or not"
    (is (= (cu/derivative derivative-test-input-1) derivative-test-output-1))
    (is (= (cu/derivative derivative-test-input-2) derivative-test-output-2))
    (is (= (cu/derivative derivative-test-input-3) derivative-test-output-3))))