
(ns clj-ml.utils.matrix
  (:import [java.util Random]))

(defn- equal-dimensions?
  "Checks if the nested matrices of a matrix have euqal dimensions or not"
  [m]
  (if (every? coll? m)
    (not (nil?
           (reduce
             (fn [acc r]
               (if (nil? acc)
                 acc
                 (when (= (count acc) (count r)) r))) m)))
    true))

(defn matrix?
  "Returns true if a data structure is a valid N-D matrix, else false"
  [m]
  (when (coll? m)
    (loop [mat m
           nested? false
           result false]
      (if (or (and nested? (false? result)) (empty? mat))
        result
        (let [f (first mat)]
          (if-not (coll? f)
            (if nested? result (pos-int? (count mat)))
            (recur (first mat) true
                   (and (equal-dimensions? mat)
                        (every? true? (map equal-dimensions? mat))))))))))

(defn get-val
  "Get's a specific value from the martix `m` based on the path provided in `index-path`"
  [m index-path]
  (loop [ip index-path
         result m]
    (if (empty? ip)
      result
      (recur (rest ip) (nth result (first ip))))))

(defmacro random-fn
  "Executes the function `f` repeatedly `n` times"
  [n f]
  `(repeatedly ~n (fn [] ~f)))

(defn create-matrix
  "Creates an N-D matrix of `dimenstions` specified in order with random float values in it.
   Optional seed (Integer Value) for the random matrix generator can be specified as well."
  [{:keys [dimensions seed] :or {seed 10}}]
  (loop [d dimensions
         result (lazy-seq [])]
    (if (empty? d)
      result
      (recur (butlast d)
             (if (seq result)
               (random-fn (last d) result)
               (concat result (random-fn (last d) (rand seed))))))))

(defn create-identity-matrix
  "Same as `clj-ml.utils.matrix/create-matrix`
   But this one creates a 2-D identity matrix"
  [m]
  (map (fn [i] (map (fn [j] (if (= i j) 1 0)) (range m))) (range m)))

(defn dimension
  "Returns the dimenston of a 2-D matrix in a vector two elements"
  [m]
  [(count m) (count (first m))])

(defn identity-matrix?
  "Checks if the matrix `m` is a 2-D identity matrix or not"
  [m]
  (let [[p q] (dimension m)]
    (and (matrix? m)
         (= p q)
         (every? true?
           (flatten
             (map (fn [i]
                    (map (fn [j]
                           (let [ij (get-val m [i j])] (if (= i j) (= 1 ij) (zero? ij))))
                         (range p)))
                  (range p)))))))

(defn transpose
  "Returns the transpose of a 2-D matrix"
  [m]
  (apply map list m))

(defn perform-arithmetic-op
  "Performs arithmetic operation on a matrix with a scalar or another matrix"
  [mat operand operation]
  (if-not (matrix? operand)
    (map (fn [row] (map (fn [col-elem] (operation col-elem operand)) row)) mat)
    (if (= (dimension mat) (dimension operand))
      (map
       (fn [row-1 row-2]
         (map
          operation
          row-1 row-2)) mat operand)
      (throw (Exception. "Dimensions of matrices are not the same")))))

(defn matrix-multiply
  "Multiplies two matrices of MxN and NxP dimensions
   Calculates the dot product"
  [a b]
  (if (= (second (dimension a)) (first (dimension b)))
    (let [b-transpose (transpose b)]
      (map
       (fn [row-a]
         (map (fn [col-b] (reduce + (map * row-a col-b))) b-transpose)) a))
    (throw (Exception. "The number of columns of the first matrix
                        are not equal to the number of rows of the second matrix"))))

(defn reciprocal
  "Calculates the reciprocal of each and every element of a 2-D matrix"
  [m]
  (map (fn [row] (map (fn [col-elem] (double (/ 1 col-elem))) row)) m))

(defn exponential
  "Calculates the exponential of each and every element of a 2-D matrix"
  [m]
  (map (fn [row] (map (fn [col-elem] (double (Math/exp col-elem))) row)) m))

(defn absolute
  "Calculates the absolute value of each and every element of a 2-D matrix"
  [m]
  (map (fn [row] (map (fn [col-elem] (double (Math/abs col-elem))) row)) m))

(defn sigmoid
  "Returns the sigmoid/logistic values of a 2-D matrix"
  ([m]
   (-> (perform-arithmetic-op m -1 *)
       exponential
       (perform-arithmetic-op 1 +)
       reciprocal))
  ([m deriv]
   (-> (perform-arithmetic-op m -1 *)
       (perform-arithmetic-op 1 +)
       (perform-arithmetic-op m *))))

(defn mean
  "Calculates the mean of a 2-D matrix"
  [m]
  (let [averaged-rows (map (fn [row] (double (/ (reduce + row) (count row)))) m)]
    (double (/ (reduce + averaged-rows) (count averaged-rows)))))
