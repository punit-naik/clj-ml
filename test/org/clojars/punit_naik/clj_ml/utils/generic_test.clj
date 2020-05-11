(ns org.clojars.punit-naik.clj-ml.utils.generic-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.generic :as gu]))

(defonce ^:private sample-identity-matrix [[1 0 0] [0 1 0] [0 0 1]])

(deftest index-matrix-rows-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/index-matrix-rows` is correctly able to index a matrix or not"
    (is (= (gu/index-matrix-rows sample-identity-matrix)
           {0 [1 0 0] 1 [0 1 0] 2 [0 0 1]}))))

(deftest first-n-zeros-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/first-n-zeros` is correctly able to find the first n zeros of a row or not"
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 0)) 0))
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 1)) 1))
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 2)) 2))))

(deftest replace-nth-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/replace-nth` properly replaces the matrix row or not"
    (is (= (gu/replace-nth sample-identity-matrix 2 [0 0 -1])
           (assoc sample-identity-matrix 2 [0 0 -1])))))

(deftest rationalise-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.matrix/rationalise` function"
    (is (= (gu/rationalise 2.54) [254 100]))
    (is (= (gu/rationalise 2.53) [253 100]))
    (is (= (gu/rationalise 2.52) [252 100]))))

(deftest shingles-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.matrix/shingles `function"
    (is (= (gu/shingles "punit" 1) ["p" "u" "n" "i" "t"]))
    (is (= (gu/shingles "punit n" 2) ["pu" "un" "ni" "it" "t " " n"]))
    (is (= (gu/shingles "punit n" 3) ["pun" "uni" "nit" "it " "t n"]))
    (is (= (gu/shingles "punit n" 6) ["punit " "unit n"]))
    (is (= (gu/shingles "punit n" 7) ["punit n"]))
    (is (= (gu/shingles "punit n" 8) []))
    (is (= (gu/shingles "punit n" 9) []))))

(deftest approximate-decimal-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.matrix/approximate-decimal` function"
    (is (= (gu/approximate-decimal 1.1234 3) 1.123))
    (is (= (gu/approximate-decimal 1.1234 2) 1.12))
    (is (= (gu/approximate-decimal 1.1234 1) 1.1))))