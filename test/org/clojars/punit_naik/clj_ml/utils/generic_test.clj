(ns org.clojars.punit-naik.clj-ml.utils.generic-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.generic :as gu]))

(defonce ^:private sample-identity-matrix [[1 0 0] [0 1 0] [0 0 1]])

(deftest index-matrix-rows-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.generic/index-matrix-rows` is correctly able to index a matrix or not"
    (is (= (gu/index-matrix-rows sample-identity-matrix)
           {0 [1 0 0] 1 [0 1 0] 2 [0 0 1]}))))

(deftest first-n-zeros-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.generic/first-n-zeros` is correctly able to find the first n zeros of a row or not"
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 0)) 0))
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 1)) 1))
    (is (= (gu/first-n-zeros (nth sample-identity-matrix 2)) 2))))

(deftest replace-nth-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.generic/replace-nth` properly replaces the matrix row or not"
    (is (= (gu/replace-nth sample-identity-matrix 2 [0 0 -1])
           (assoc sample-identity-matrix 2 [0 0 -1])))))

(deftest rationalise-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.generic/rationalise` function"
    (is (= (gu/rationalise 2.54) [254 100]))
    (is (= (gu/rationalise 2.53) [253 100]))
    (is (= (gu/rationalise 2.52) [252 100]))))

(deftest shingles-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.generic/shingles `function"
    (is (= (gu/shingles "punit" 1) ["p" "u" "n" "i" "t"]))
    (is (= (gu/shingles "punit n" 2) ["pu" "un" "ni" "it" "t " " n"]))
    (is (= (gu/shingles "punit n" 3) ["pun" "uni" "nit" "it " "t n"]))
    (is (= (gu/shingles "punit n" 6) ["punit " "unit n"]))
    (is (= (gu/shingles "punit n" 7) ["punit n"]))
    (is (= (gu/shingles "punit n" 8) []))
    (is (= (gu/shingles "punit n" 9) []))))

(deftest approximate-decimal-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.generic/approximate-decimal` function"
    (is (= (gu/approximate-decimal 1.1234 3) 1.123))
    (is (= (gu/approximate-decimal 1.1234 2) 1.12))
    (is (= (gu/approximate-decimal 1.1234 1) 1.1))))

(deftest error-decimal-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.generic/error-decimal` function"
    (is (= (gu/error-decimal 1) 0.01))
    (is (= (gu/error-decimal 2) 0.001))
    (is (= (gu/error-decimal 3) 0.0001))))

(deftest round-decimal-test
  (testing "the `org.clojars.punit-naik.clj-ml.utils.generic/round-decimal` function"
    (is (= (gu/round-decimal 1.79) 1.79))
    (is (= (gu/round-decimal 1.99) 1.99))
    (is (= (gu/round-decimal 1.999) 1.999))
    (is (= (gu/round-decimal 1.9999) 1.9999))
    (is (= (gu/round-decimal 1.99999) 2.0))
    (is (= (gu/round-decimal 2.9999999002342399) 3.0))
    (is (= (gu/round-decimal -0.9993960000000004) -1.0))
    (is (= (gu/round-decimal -1.00001) -1.0))))
