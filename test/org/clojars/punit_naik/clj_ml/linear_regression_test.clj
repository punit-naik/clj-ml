(ns org.clojars.punit-naik.clj-ml.linear-regression-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.linear-regression :as lin-reg]))

(def data-1 [[1 1] [2 3] [3 3] [4 5]])
(def input-matrix-data-1 '((1 1) (1 2) (1 3) (1 4)))
(def output-matrix-data-1 '((1) (3) (3) (5)))
(def betas-data-1 '(0.0 1.2000000000000002))
(def data-2 [[1 1] [2 2] [3 3] [4 4]])
(def input-matrix-data-2 '((1 1) (1 2) (1 3) (1 4)))
(def output-matrix-data-2 '((1) (2) (3) (4)))
(def betas-data-2 '(0.0 1.0))
(def data-3 [[1 1 1] [2 2 2] [3 3 3] [4 4 4] [5 5 5]])
(def input-matrix-data-3 '((1 1 1) (1 2 2) (1 3 3) (1 4 4) (1 5 5)))
(def output-matrix-data-3 '((1) (2) (3) (4) (5)))

(deftest generate-input-matrix-test
  (testing "If the function correctly generates input matrix for linear regression"
    (is (= (lin-reg/generate-input-matrix data-1) input-matrix-data-1))
    (is (= (lin-reg/generate-input-matrix data-2) input-matrix-data-2))
    (is (= (lin-reg/generate-input-matrix data-3) input-matrix-data-3))))

(deftest generate-output-matrix-test
  (testing "If the function correctly generates output matrix for linear regression"
    (is (= (lin-reg/generate-output-matrix data-1) output-matrix-data-1))
    (is (= (lin-reg/generate-output-matrix data-2) output-matrix-data-2))
    (is (= (lin-reg/generate-output-matrix data-3) output-matrix-data-3))))

(deftest betas-test
  (testing "If the beta values for data are generated correctly"
    (is (= (lin-reg/betas :matrix data-1) betas-data-1))
    (is (= (lin-reg/betas :matrix data-2) betas-data-2))))

(deftest predict-test
  (testing "If the output value is being correctly predicted by given beta values and test data"
    (is (= (lin-reg/predict betas-data-1 [5]) 6.000000000000001))
    (is (= (lin-reg/predict betas-data-2 [5]) 5.0))))