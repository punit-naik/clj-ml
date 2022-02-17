(ns org.clojars.punit-naik.clj-ml.utils.matrix-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.matrix :as mu]))

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
(defonce ^:private mat-mul-error-str "The number of columns of the first matrix are not equal to the number of rows of the second matrix")
(defonce ^:private perform-arithmetic-op-error-str "Dimensions of matrices are not the same")
(defonce ^:private covar-mat-in-1 [[1 1 1] [1 2 1] [1 3 2] [1 4 3]])
(defonce ^:private covar-mat-out-1 '((0.0 0.0 0.0) (0.0 1.6666666666666667 1.1666666666666667) (0.0 0.0 0.9166666666666666)))
(defonce ^:private covar-mat-in-2 [[2.1 8] [2.5 10] [3.6 12] [4 14]])
(defonce ^:private covar-mat-out-2 '((0.8033333333333333 2.2666666666666666) (0.0 6.666666666666667)))
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
(defonce ^:private sigmoid-input-1 [[0.2 0.4] [0.5 0.7] [0.9 0.004]])
(defonce ^:private sigmoid-output-1 '((0.549833997312478 0.598687660112452)
                                      (0.6224593312018546 0.6681877721681662)
                                      (0.7109495026250039 0.5009999986666688)))
(defonce ^:private sigmoid-input-2 sample-identity-matrix)
(defonce ^:private sigmoid-output-2 '((0.7310585786300049 0.5 0.5)
                                      (0.5 0.7310585786300049 0.5)
                                      (0.5 0.5 0.7310585786300049)))
(defonce ^:private concat-matrix-rows-output-1 '((1 0 0 1 0) (0 1 0 0 1) (0 0 1 0 0)))
(defonce ^:private concat-matrix-rows-output-2 '((3 -2 5 3 -2) (6 -4 7 6 -4) (5 -4 6 5 -4)))
(defonce ^:private characteristic-equation-parts-output-1 [-1 3 -3 1])
(defonce ^:private characteristic-equation-parts-output-2 [-1 6 -12 8])
(defonce ^:private characteristic-equation-parts-output-3 [-1 36 -188 240])
(defonce ^:private matrix-inverse-input-1 [[1 -2 3] [3 5 2] [-1 3 -4]])
(defonce ^:private matrix-inverse-output-1 '((6.5 -0.25 4.75) (-2.5 0.25 -1.75) (-3.5 0.25 -2.75)))

(deftest equal-dimensions?-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/equal-dimensions?` function"
    (testing "correctly identifies a matrix with equal dimensions"
      (is (mu/equal-dimensions? valid-2d-matrix)))
    (testing "correctly identifies an matrix with unequal dimensions"
      (is (false? (mu/equal-dimensions? invalid-2d-matrix))))))

(deftest is-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/matrix?` function"
    (testing "correctly identifies a valid matrix"
      (is (mu/matrix? valid-2d-matrix)))
    (testing "correctly identifies an invalid matrix"
      (is (false? (mu/matrix? invalid-2d-matrix))))))

(deftest get-val-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/get-val` function works properly"
    (is (= (mu/get-val sample-identity-matrix [0]) [1 0 0]))
    (is (= (mu/get-val sample-identity-matrix [1]) [0 1 0]))
    (is (= (mu/get-val sample-identity-matrix [2]) [0 0 1]))
    (is (= (mu/get-val sample-identity-matrix [0 0]) 1))
    (is (= (mu/get-val sample-identity-matrix [1 1]) 1))
    (is (= (mu/get-val sample-identity-matrix [2 2]) 1))))

(deftest index-matrix-rows-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/index-matrix-rows` is correctly able to index a matrix or not"
    (is (= (mu/index-matrix-rows sample-identity-matrix)
           {0 [1 0 0] 1 [0 1 0] 2 [0 0 1]}))))

(deftest random-fn-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/random-fn` function works properly"
    (let [seed 10
          checker-fn (fn [coll] (every? (fn [x] (<= 0 x seed)) coll))]
      (is (checker-fn (mu/random-fn 5 (rand seed))))
      (is (checker-fn (mu/random-fn 10 (rand seed))))
      (is (checker-fn (mu/random-fn 15 (rand seed)))))))

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

(deftest dimension-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/dimension` function finds the dimensions of a mtrix properly"
    (is (= (mu/dimension valid-2d-matrix) [2 3]))
    (is (= (mu/dimension sample-identity-matrix) [3 3]))))

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
      (is (= (mu/perform-arithmetic-op sample-identity-matrix valid-2d-matrix-2 /) valid-2d-matrix-divied-by-identity-matrix))
      (try (mu/perform-arithmetic-op valid-2d-matrix-2 valid-2d-matrix -)
           (catch AssertionError e
             (is (= (.getMessage e) perform-arithmetic-op-error-str)))))))

(deftest matrix-multiply-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/matrix-multiply` calculates the dot product of two matrices properly"
    (is (= (mu/matrix-multiply valid-2d-matrix-2 sample-identity-matrix) valid-2d-matrix-2))
    (try (mu/matrix-multiply valid-2d-matrix-2 valid-2d-matrix)
      (catch AssertionError e
        (is (= (.getMessage e) mat-mul-error-str))))))

(deftest mean-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/mean` function calculates the mean of a 2D matrix properly"
    (is (= (format "%.2f"(mu/mean sample-identity-matrix)) "0.33"))))

(deftest absolute-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/absolute` function calculates the absolute of a 2D matrix properly"
    (is (= (mu/absolute sample-identity-matrix-negative-values) (mu/perform-arithmetic-op sample-identity-matrix 1.0 *)))))

(deftest sigmoid-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/sigmoid` function calculates the sigmoid of a 2D matrix properly"
    (is (= (mu/sigmoid sigmoid-input-1) sigmoid-output-1))
    (is (= (mu/sigmoid sigmoid-input-2) sigmoid-output-2))))

(deftest reciprocal-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/reciprocal` function calculates the reciprocal of a 2D matrix properly"
    (is (= (mu/reciprocal valid-2d-matrix) valid-2d-matrix-reciprocal))))

(deftest exponential-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/exponential` function calculates the exponential of a 2D matrix properly"
    (is (= (mu/exponential valid-2d-matrix) valid-2d-matrix-exponential))))

(deftest covariance-matrix-test
  (testing "Checking if the `org.clojars.punit-naik.clj-ml.utils.matrix/covariance` function calculates the covariance matrix of a 2D matrix properly"
    (is (= (mu/covariance covar-mat-in-1) covar-mat-out-1))
    (is (= (mu/covariance covar-mat-in-2) covar-mat-out-2))))

(deftest triangular-matrix?-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/triangular-matrix?` properly identifies an upper triangular matrix or not"
    (is (mu/triangular-matrix? sample-identity-matrix))
    (is (mu/triangular-matrix? [[0 0] [0 0]]))
    (is (mu/triangular-matrix? [[0 1] [1 0]]))
    (is (not (mu/triangular-matrix? valid-2d-matrix-2)))))

(deftest row-adjust-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/row-adjust` properly adjusts the row or not"
    (is (= (mu/row-adjust [1 3 1 4] [3 9 5 15] 2) '(0.0 0.0 2.0 3.0)))
    (is (= (mu/row-adjust [0 2 1 1] [0 4 2 3] 2) '(0.0 0.0 0.0 1.0)))))

(deftest recursive-row-adjust-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/recursive-row-adjust` properly adjusts the rows of a matrix or not"
    (is (= (mu/recursive-row-adjust upper-triangular-matrix-data-1 1) '(0.0 0.0 -3.0)))
    (is (= (mu/recursive-row-adjust upper-triangular-matrix-data-1 2) '(0.0 -0.6666666666666665 -2.333333333333334)))))

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

(deftest concat-matrix-rows-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/concat-matrix-rows` correctly works or not"
    (is (= (mu/concat-matrix-rows sample-identity-matrix 3) concat-matrix-rows-output-1))
    (is (= (mu/concat-matrix-rows upper-triangular-matrix-data-1 3) concat-matrix-rows-output-2))))

(deftest characteristic-equation-parts-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/characteristic-equation-parts` correctly works or not"
    (is (= (mu/characteristic-equation-parts (mu/concat-matrix-rows (mu/matrix-minus-lambda-i sample-identity-matrix) 3) 3)
           characteristic-equation-parts-output-1))
    (is (= (mu/characteristic-equation-parts (mu/concat-matrix-rows (mu/matrix-minus-lambda-i eigen-value-data-1) 3) 3)
           characteristic-equation-parts-output-2))
    (is (= (mu/characteristic-equation-parts (mu/concat-matrix-rows (mu/matrix-minus-lambda-i eigen-value-data-2) 3) 3)
           characteristic-equation-parts-output-3))))

(deftest matrix-minus-lambda-i-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/matrix-minus-lambda-i` correctly works correctly or not"
    (is (= (mu/matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 0.5857864376269049) '([1.4142135623730951 1 0] [1 1.4142135623730951 1] [0 1 1.4142135623730951])))
    (is (= (mu/matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 3.414213562373095) '([-1.414213562373095 1 0] [1 -1.414213562373095 1] [0 1 -1.414213562373095])))
    (is (= (mu/matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 2) '([0 1 0] [1 0 1] [0 1 0])))))

(deftest eigen-values-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/eigen-values` correctly calculates the eigen values of a matrix or not"
    (is (= (mu/eigen-values sample-identity-matrix) '(1 1 1)))
    (is (= (mu/eigen-values eigen-value-data-1) eigen-value-result-1))
    (is (= (mu/eigen-values eigen-value-data-2) eigen-value-result-2))
    (is (= (mu/eigen-values covar-mat-out-2) '(0.8033333333333333 6.666666666666667)))))

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

(deftest adjust-rref-indices-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/adjust-rref-indices` correctly works"
    (is (= (mu/adjust-rref-indices 3 [[0 1 0] [0 0 1] [0 0 0]]) '([0 0 0] [0 1 0] [0 0 1])))
    (is (= (mu/adjust-rref-indices 4 [[0 0 0 1] [0 1 0 0] [1 0 0 0] [0 0 0 0]]) '([1 0 0 0] [0 1 0 0] [0 0 0 0] [0 0 0 1])))))

(deftest eigen-vectors-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/eigen-vectors` correctly finds the eigen vectors of a matrix or not"
    (is (= (mu/eigen-vectors eigen-value-data-2 (mu/eigen-values eigen-value-data-2)) ['(135.0 -10.0 1.0) '(1.0 -0.0 -0.0) '(1.125 1.3333333333333333 1.0)]))
    (is (= (mu/eigen-vectors eigen-vector-input-1 (mu/eigen-values eigen-vector-input-1)) ['(-2.0 1.0 1.0) '(0.5 1.0 0.0) '(0.5 0.0 1.0)]))
    (is (= (mu/eigen-vectors eigen-vector-input-2 (mu/eigen-values eigen-vector-input-2)) ['(1.0 -1.41422 1.0) '(-1.0 -0.0 1.0) '(1.0 1.4142100000000002 1.0)]))
    (is (= (mu/eigen-vectors covar-mat-out-2 (mu/eigen-values covar-mat-out-2)) [[1.0 -0.0] [0.3865832859579306 1.0]]))))

(deftest inverse-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.matrix/inverse` correctly finds the inverse of a matrix or not"
    (is (and (= (mu/inverse matrix-inverse-input-1) matrix-inverse-output-1)
             (mu/identity-matrix? (mu/matrix-multiply matrix-inverse-input-1 matrix-inverse-output-1))))))