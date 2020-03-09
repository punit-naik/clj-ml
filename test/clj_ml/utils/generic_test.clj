(ns clj-ml.utils.generic-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-ml.utils.generic :as gu]))

(defonce ^:private sample-identity-matrix [[1 0 0] [0 1 0] [0 0 1]])

(deftest index-matrix-rows-test
  (testing "If the function `clj-ml.utils.matrix/index-matrix-rows` is correctly able to index a matrix or not"
    (is (= (gu/index-matrix-rows sample-identity-matrix)
           {0 [1 0 0] 1 [0 1 0] 2 [0 0 1]}))))

(deftest first-n-zeros-test
  (testing "If the function `clj-ml.utils.matrix/first-n-zeros` is correctly able to find the first n zeros of a row or not"
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 0)) 0))
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 1)) 1))
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 2)) 2))))

(deftest replace-nth-test
  (testing "If the function `clj-ml.utils.matrix/replace-nth` properly replaces the matrix row or not"
    (is (= (gu/replace-nth sample-identity-matrix 2 [0 0 -1])
           (assoc sample-identity-matrix 2 [0 0 -1])))))