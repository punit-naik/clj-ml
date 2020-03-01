(ns clj-ml.utils.matrix-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clj-ml.utils.matrix :refer :all]
            [clojure.string :as clj-str])
  (:import [clojure.lang LazySeq]))

(defonce ^:private sample-identity-matrix [[1 0 0] [0 1 0] [0 0 1]])
(defonce ^:private sample-identity-matrix-negative-values [[-1 0 0] [0 -1 0] [0 0 -1]])
(defonce ^:private valid-2d-matrix [[1 2 3] [4 5 6]])
(defonce ^:private valid-2d-matrix-reciprocal '((1.0 0.5 0.3333333333333333) (0.25 0.2 0.1666666666666667)))
(defonce ^:private valid-2d-matrix-exponential '((2.718281828459045 7.38905609893065 20.085536923187668) (54.598150033144236 148.4131591025766 403.4287934927351)))
(defonce ^:private valid-2d-matrix-2 [[1 2 3] [4 5 6] [7 8 9]])
(defonce ^:private valid-2d-matrix-added-to-identity-matrix [[2 2 3] [4 6 6] [7 8 10]])
(defonce ^:private valid-2d-matrix-subtracted-from-identity-matrix [[-0 -2 -3] [-4 -4 -6] [-7 -8 -8]])
(defonce ^:private valid-2d-matrix-divied-by-identity-matrix [[1 0 0] [0 (/ 1 5) 0] [0 0 (/ 1 9)]])
(defonce ^:private valid-2d-matrix-multiplied-by-identity-matrix [[1 0 0] [0 5 0] [0 0 9]])
(defonce ^:private valid-2d-matrix-each-elem-multipled-by-2 [[2 4 6] [8 10 12]])
(defonce ^:private valid-2d-matrix-each-elem-divied-by-2 [[(/ 1 2) 1 (/ 3 2)] [2 (/ 5 2) 3]])
(defonce ^:private valid-2d-matrix-each-elem-added-by-2 [[3 4 5] [6 7 8]])
(defonce ^:private valid-2d-matrix-each-elem-subtracted-by-2 [[-1 0 1] [2 3 4]])
(defonce ^:private valid-2d-matrix-transposed [[1 4] [2 5] [3 6]])
(defonce ^:private invalid-2d-matrix [[1 2 3] [4 6]])
(defonce ^:private mat-mul-error-str "The number of columns of the first matrixare not equal to the number of rows of the second matrix")

(deftest is-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/matrix?` function"
    (testing "correctly identifies a valid matrix"
      (is (matrix? valid-2d-matrix)))
    (testing "correctly identifies an invalid matrix"
      (is (false? (matrix? invalid-2d-matrix))))))

(deftest create-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/create-matrix` function creates a valid 2D matrix"
    (let [m (create-matrix {:dimensions [2 3]})]
      (is (seq m))
      (is (matrix? m))
      (is (= (dimension m) [2 3])))
    (testing " (square)"
     (let [m (create-matrix {:dimensions [3 3]})]
       (is (seq m))
       (is (matrix? m))
       (is (= (dimension m) [3 3]))))))

(deftest create-identity-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/create-identity-matrix` function creates a valid 2D identity matrix"
    (let [m (create-identity-matrix 3)]
      (is (seq m))
      (is (identity-matrix? m))
      (is (= (dimension m) [3 3]))
      (is (= m sample-identity-matrix)))))

(deftest transpose-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/transpose` function transposes a 2D matrix properly"
    (is (= (transpose valid-2d-matrix) valid-2d-matrix-transposed))))

(deftest perform-arithmetic-op-test
  (testing "Checking if the `clj-ml.utils.matrix/perform-arithmetic-op` function performs arithmetic operations on the matrix properly"
    (testing "when performing operations with a scalar"
      (is (= (perform-arithmetic-op valid-2d-matrix 2 *) valid-2d-matrix-each-elem-multipled-by-2))
      (is (= (perform-arithmetic-op valid-2d-matrix 2 -) valid-2d-matrix-each-elem-subtracted-by-2))
      (is (= (perform-arithmetic-op valid-2d-matrix 2 +) valid-2d-matrix-each-elem-added-by-2))
      (is (= (perform-arithmetic-op valid-2d-matrix 2 /) valid-2d-matrix-each-elem-divied-by-2)))
    (testing "when performing operations with another matrix"
      (is (= (perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 +) valid-2d-matrix-added-to-identity-matrix))
      (is (= (perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 -) valid-2d-matrix-subtracted-from-identity-matrix))
      (is (= (perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 *) valid-2d-matrix-multiplied-by-identity-matrix))
      (is (= (perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 /) valid-2d-matrix-divied-by-identity-matrix)))))

(deftest matrix-multiply-test
  (testing "If the function `clj-ml.utils.matrix/matrix-multiply` calculates the dot product of two matrices properly"
    (is (= (matrix-multiply valid-2d-matrix-2 sample-identity-matrix) valid-2d-matrix-2))
    (try (matrix-multiply valid-2d-matrix-2 valid-2d-matrix)
      (catch Exception e
        (is (= (clj-str/replace (.getMessage e) #"\n|\s\s+" "") mat-mul-error-str))))))

(deftest mean-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/mean` function calculates the mean of a 2D matrix properly"
    (is (= (format "%.2f"(mean sample-identity-matrix)) "0.33"))))

(deftest absolute-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/absolute` function calculates the absolute of a 2D matrix properly"
    (is (= (absolute sample-identity-matrix-negative-values) (perform-arithmetic-op sample-identity-matrix 1.0 *)))))

(deftest reciprocal-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/reciprocal` function calculates the reciprocal of a 2D matrix properly"
    (is (= (reciprocal valid-2d-matrix) valid-2d-matrix-reciprocal))))

(deftest exponential-matrix-test
  (testing "Checking if the `clj-ml.utils.matrix/exponential` function calculates the exponential of a 2D matrix properly"
    (is (= (exponential valid-2d-matrix) valid-2d-matrix-exponential))))
