(ns org.clojars.punit-naik.clj-ml.utils.matrix-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.matrix :as mu]
            [clojure.string :as clj-str]))

(defonce ^:private sample-identity-matrix [[1 0 0] [0 1 0] [0 0 1]])
(defonce ^:private sample-identity-matrix-swapped-1 '([0 1 0] [1 0 0] [0 0 1]))
(defonce ^:private sample-identity-matrix-swapped-2 '([0 0 1] [0 1 0] [1 0 0]))
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
(defonce ^:private covar-mat-in [[1 1 1] [1 2 1] [1 3 2] [1 4 3]])
(defonce ^:private covar-mat-out '((0.0 0.0 0.0) (0.0 1.6666666666666667 1.1666666666666667) (0.0 0.0 0.9166666666666666)))
(defonce ^:private upper-triangular-matrix-data-1 [[3 -2 5] [6 -4 7] [5 -4 6]])
(defonce ^:private upper-triangular-matrix-data-1-result '([3 -2 5] (0.0 -0.6666666666666665 -2.333333333333334) (0.0 0.0 -3.0)))
(defonce ^:private upper-triangular-matrix-data-2 [[1 3 1 4] [3 9 5 15] [0 2 1 1] [0 4 2 3]])
(defonce ^:private upper-triangular-matrix-data-2-result '([1 3 1 4] [0 2 1 1] (0.0 0.0 2.0 3.0) (0.0 0.0 0.0 1.0)))
(defonce ^:private upper-triangular-matrix-data-3 [[3 0 0 3 0] [-3 0 -2 0 0] [0 -1 0 0 -3] [0 0 0 3 3] [0 -1 2 0 1]])
(defonce ^:private upper-triangular-matrix-data-3-result '([3 0 0 3 0] [0 -1 0 0 -3] (0.0 0.0 -2.0 3.0 0.0) [0 0 0 3 3] (0.0 0.0 0.0 0.0 1.0)))
(defonce ^:private upper-triangular-matrix-data-4 [[13 2 -18] [14 1 -18] [10 2 -15]])
(defonce ^:private upper-triangular-matrix-data-4-result '([13 2 -18] (0.0 -1.1538461538461537 1.3846153846153832) (0.0 0.0 -0.6000000000000001)))
(defonce ^:private cross-product-data-1 {0 -1 1 1})
(defonce ^:private cross-product-data-2 {'(0 0) 1, '(0 1) -2, '(1 1) 1})
(defonce ^:private cross-product-data-3 {'(0 0 0) -1, '(0 0 1) 3, '(0 1 1) -3, '(1 1 1) 1})
(defonce ^:private eigen-value-data-1 [[2 1 0] [1 2 1] [0 1 2]])
(defonce ^:private eigen-value-result-1 '(0.58578 2.0 3.41421))
(defonce ^:private eigen-value-data-2 [[2 27 0] [0 4 40] [0 3 30]])
(defonce ^:private eigen-value-result-2 '(0 2.0 34.0))
(defonce ^:private eigen-vector-input-1 [[-1 2 2] [2 2 -1] [2 -1 2]])
(defonce ^:private eigen-vector-input-2 [[2 1 0] [1 2 1] [0 1 2]])

(deftest is-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/matrix?` function"
    (testing "correctly identifies a valid matrix"
      (is (mu/matrix? valid-2d-matrix)))
    (testing "correctly identifies an invalid matrix"
      (is (false? (mu/matrix? invalid-2d-matrix))))))

(deftest create-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/create-matrix` function creates a valid 2D matrix"
    (let [m (mu/create-matrix {:dimensions [2 3]})]
      (is (seq m))
      (is (mu/matrix? m))
      (is (= (mu/dimension m) [2 3])))
    (testing " (square)"
     (let [m (mu/create-matrix {:dimensions [3 3]})]
       (is (seq m))
       (is (mu/matrix? m))
       (is (= (mu/dimension m) [3 3]))))))

(deftest create-identity-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/create-identity-matrix` function creates a valid 2D identity matrix"
    (let [m (mu/create-identity-matrix 3)]
      (is (seq m))
      (is (mu/identity-matrix? m))
      (is (= (mu/dimension m) [3 3]))
      (is (= m sample-identity-matrix)))))

(deftest transpose-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/transpose` function transposes a 2D matrix properly"
    (is (= (mu/transpose valid-2d-matrix) valid-2d-matrix-transposed))))

(deftest perform-arithmetic-op-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/perform-arithmetic-op` function performs arithmetic operations on the matrix properly"
    (testing "when performing operations with a scalar"
      (is (= (mu/perform-arithmetic-op valid-2d-matrix 2 *) valid-2d-matrix-each-elem-multipled-by-2))
      (is (= (mu/perform-arithmetic-op valid-2d-matrix 2 -) valid-2d-matrix-each-elem-subtracted-by-2))
      (is (= (mu/perform-arithmetic-op valid-2d-matrix 2 +) valid-2d-matrix-each-elem-added-by-2))
      (is (= (mu/perform-arithmetic-op valid-2d-matrix 2 /) valid-2d-matrix-each-elem-divied-by-2)))
    (testing "when performing operations with another matrix"
      (is (= (mu/perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 +) valid-2d-matrix-added-to-identity-matrix))
      (is (= (mu/perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 -) valid-2d-matrix-subtracted-from-identity-matrix))
      (is (= (mu/perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 *) valid-2d-matrix-multiplied-by-identity-matrix))
      (is (= (mu/perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 /) valid-2d-matrix-divied-by-identity-matrix)))))

(deftest matrix-multiply-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/matrix-multiply` calculates the dot product of two matrices properly"
    (is (= (mu/matrix-multiply valid-2d-matrix-2 sample-identity-matrix) valid-2d-matrix-2))
    (try (mu/matrix-multiply valid-2d-matrix-2 valid-2d-matrix)
      (catch Exception e
        (is (= (clj-str/replace (.getMessage e) #"\n|\s\s+" "") mat-mul-error-str))))))

(deftest mean-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/mean` function calculates the mean of a 2D matrix properly"
    (is (= (format "%.2f"(mu/mean sample-identity-matrix)) "0.33"))))

(deftest absolute-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/absolute` function calculates the absolute of a 2D matrix properly"
    (is (= (mu/absolute sample-identity-matrix-negative-values) (mu/perform-arithmetic-op sample-identity-matrix 1.0 *)))))

(deftest reciprocal-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/reciprocal` function calculates the reciprocal of a 2D matrix properly"
    (is (= (mu/reciprocal valid-2d-matrix) valid-2d-matrix-reciprocal))))

(deftest exponential-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/exponential` function calculates the exponential of a 2D matrix properly"
    (is (= (mu/exponential valid-2d-matrix) valid-2d-matrix-exponential))))

(deftest covariance-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/covariance` function calculates the covariance matrix of a 2D matrix properly"
    (is (= (mu/covariance covar-mat-in) covar-mat-out))))

(deftest triangular-matrix?-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/triangular-matrix?` properly identifies an upper triangular matrix or not"
    (is (mu/triangular-matrix? sample-identity-matrix))
    (is (mu/triangular-matrix? [[0 0] [0 0]]))
    (is (not (mu/triangular-matrix? valid-2d-matrix-2)))))

(deftest row-adjust-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/row-adjust` properly adjusts the row or not"
    (is (= (mu/row-adjust [1 3 1 4] [3 9 5 15] 2) '(0.0 0.0 2.0 3.0)))
    (is (= (mu/row-adjust [0 2 1 1] [0 4 2 3] 2) '(0.0 0.0 0.0 1.0)))))

(deftest upper-triangular-matrix-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/upper-triangular-matrix` properly generates an upper triangular matrix or not"
    (is (= (:upper-triangular (mu/upper-triangular-matrix sample-identity-matrix)) sample-identity-matrix))
    (is (= (:upper-triangular (mu/upper-triangular-matrix upper-triangular-matrix-data-1)) upper-triangular-matrix-data-1-result))
    (is (= (:upper-triangular (mu/upper-triangular-matrix upper-triangular-matrix-data-2)) upper-triangular-matrix-data-2-result))
    (is (= (:upper-triangular (mu/upper-triangular-matrix upper-triangular-matrix-data-3)) upper-triangular-matrix-data-3-result))
    (is (= (:upper-triangular (mu/upper-triangular-matrix upper-triangular-matrix-data-4)) upper-triangular-matrix-data-4-result))))

(deftest determinant-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/determinant` properly generates an upper triangular matrix or not"
    (is (= (mu/determinant sample-identity-matrix) 1.0))
    (is (= (mu/determinant upper-triangular-matrix-data-1) -6.0))
    (is (= (mu/determinant upper-triangular-matrix-data-2) -4.0))
    (is (= (mu/determinant upper-triangular-matrix-data-3) -18.0))
    (is (= (mu/determinant upper-triangular-matrix-data-4) 9.0))))

(deftest swap-rows-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/swap-rows` correctly swaps the rows of a matrix or not"
    (is (= (mu/swap-rows sample-identity-matrix 0 1) sample-identity-matrix-swapped-1))
    (is (= (mu/swap-rows sample-identity-matrix 0 2) sample-identity-matrix-swapped-2))))

(deftest cross-product-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/cross-product` correctly calculates the cross product of the rows of a matrix or not"
    (is (= (mu/cross-product cross-product-data-1 cross-product-data-1) cross-product-data-2))
    (is (= (mu/cross-product cross-product-data-2 cross-product-data-1) cross-product-data-3))))

(deftest matrix-minus-lambda-i-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/matrix-minus-lambda-i` correctly works correctly or not"
    (is (= (mu/matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 0.5857864376269049) '([1.4142135623730951 1 0] [1 1.4142135623730951 1] [0 1 1.4142135623730951])))
    (is (= (mu/matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 3.414213562373095) '([-1.414213562373095 1 0] [1 -1.414213562373095 1] [0 1 -1.414213562373095])))
    (is (= (mu/matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 2) '([0 1 0] [1 0 1] [0 1 0])))))

(deftest eigen-values-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/eigen-values` correctly calculates the eigen values of a matrix or not"
    (is (= (mu/eigen-values sample-identity-matrix) '(1.0 1.0)))
    (is (= (mu/eigen-values eigen-value-data-1) eigen-value-result-1))
    (is (= (mu/eigen-values eigen-value-data-2) eigen-value-result-2))))

(deftest row-adjust-rref-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/row-adjust-rref` correctly works"
    (is (= (mu/row-adjust-rref [1.414 1 0] [1 1.414 1] 0) '(0.0 -1.0 -1.414)))
    (is (= (mu/row-adjust-rref [0 1 1.414] [1 1.414 1] 0) [0 1 1.414]))))

(deftest zero-above-below-i-j-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/zero-above-below-i-j` correctly works"
    (is (= (mu/zero-above-below-i-j [[1 1.414 1] [1.414 1 0] [0 1 1.414]] [0 0] 3) '([1 1.414 1] (0.0 -1.0 -1.414) [0 1 1.414])))
    (is (= (mu/zero-above-below-i-j '([1 -1.414 1] [-1.414 1 0] [0 1 -1.414]) [0 0] 3) '([1 -1.414 1] (0.0 -1.0 1.414) [0 1 -1.414])))))

(deftest reduced-row-echelon-form-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/reduced-row-echelon-form` correctly works"
    (is (= (mu/reduced-row-echelon-form [[-2 -2 -2] [-2 -5 1] [-2 1 -5]]) '((1.0 0.0 2.0) (0.0 1.0 -1.0) (0.0 0.0 0.0))))
    (is (= (mu/reduced-row-echelon-form '([-3 -6 3] [3 6 -3] [0 0 0])) '((1.0 2.0 -1.0) (0.0 0.0 0.0) [0 0 0])))
    (is (= (mu/reduced-row-echelon-form '([-3 -6 3] [3 6 -3] [0 0 0])) '((1.0 2.0 -1.0) (0.0 0.0 0.0) [0 0 0])))
    (is (= (mu/reduced-row-echelon-form [[1 1.414 1] [1.414 1 0] [0 1 1.414]]) '((1.0 0.0 -1.0) (-0.0 1.0 1.414) (0.0 0.0 0.0))))
    (is (= (mu/reduced-row-echelon-form [[1 -1.414 1] [-1.414 1 0] [0 1 -1.414]]) '((1.0 0.0 -1.0) (-0.0 1.0 -1.414) (0.0 0.0 0.0))))))

(deftest eigen-vector-for-lamba-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/eigen-vector-for-lamba` correctly works"
    (is (= (mu/eigen-vector-for-lamba [[2 1 0] [1 2 1] [0 1 2]] 0.5857864376269049) '(1.0 -1.4142135623730951 1.0)))
    (is (= (mu/eigen-vector-for-lamba [[2 1 0] [1 2 1] [0 1 2]] 3.414213562373095) '(1.0 1.414213562373095 1.0)))
    (is (= (mu/eigen-vector-for-lamba [[2 1 0] [1 2 1] [0 1 2]] 2) '(-1.0 -0.0 1.0)))
    (is (= (mu/eigen-vector-for-lamba eigen-vector-input-1 -3) '(-2.0 1.0 1.0)))
    (is (= (mu/eigen-vector-for-lamba eigen-vector-input-1 3) '(0.5 1.0 0.0)))
    (is (= (mu/eigen-vector-for-lamba eigen-vector-input-1 3 true) '(0.5 0.0 1.0)))))

(deftest eigen-vectors-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/eigen-vectors` correctly find the eigen vectors of a matrix or not"
    (is (= (mu/eigen-vectors eigen-value-data-2 (mu/eigen-values eigen-value-data-2)) ['(135.0 -10.0 1.0) '(1.0 -0.0 -0.0) '(1.125 1.3333333333333333 1.0)]))
    (is (= (mu/eigen-vectors eigen-vector-input-1 (mu/eigen-values eigen-vector-input-1)) ['(-2.0 1.0 1.0) '(0.5 1.0 0.0) '(0.5 0.0 1.0)]))
    (is (= (mu/eigen-vectors eigen-vector-input-2 (mu/eigen-values eigen-vector-input-2)) ['(1.0 -1.41422 1.0) '(-1.0 -0.0 1.0) '(1.0 1.4142100000000002 1.0)]))))
