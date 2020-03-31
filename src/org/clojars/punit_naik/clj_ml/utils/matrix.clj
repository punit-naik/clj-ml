(ns org.clojars.punit-naik.clj-ml.utils.matrix
  (:require [org.clojars.punit-naik.clj-ml.utils.generic :as gu]
            [org.clojars.punit-naik.clj-ml.utils.linear-algebra :as lau]))

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
  "Same as `org.clojars.punit-naik.clj-ml.utils.matrix/create-matrix`
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

(defn swap-rows
  "Swaps the rows at the `i`th and `j`th indexes"
  [m i j]
  (let [ith-row (get-val m [i])
        jth-row (get-val m [j])]
    (-> m
        (gu/replace-nth i jth-row)
        (gu/replace-nth j ith-row))))

(defn mean-coll
  "Calculates the mean of a collection `c`"
  [c]
  (double (/ (reduce + c) (count c))))

(defn mean
  "Calculates the mean of a 2-D matrix"
  [m]
  (let [averaged-rows (map (fn [row] (double (/ (reduce + row) (count row)))) m)]
    (mean-coll averaged-rows)))

(defn covariance
  "Calculates the covariance matrix of a 2-D matrix"
  [m]
  (let [n (count m)
        t (map (fn [i v] {:row i :value v}) (range n) (transpose m))]
    (loop [tm t
           result (lazy-seq [])]
      (if (empty? tm)
        (let [intermediate-result (reduce merge result)]
          (reduce (fn [acc v]
                    (concat acc [(map #(get intermediate-result % 0.0) v)]))
                  (lazy-seq [])
                  (map (fn [i]
                         (map (fn [j] [i j])
                              (range (count t))))
                       (range (count t)))))
        (let [{:keys [row value]} (first tm)
              i-mean (mean-coll value)]
          (recur (rest tm)
                 (concat result
                         (map
                           (fn [x]
                             (let [j (:value (get-val t [x]))
                                   j-mean (mean-coll j)]
                               (assoc {} (sort [row x])
                                      (double (/ (reduce +
                                                   (map #(* (- %1 i-mean) (- %2 j-mean))
                                                        value j))
                                                 (dec n))))))
                           (range row (count t))))))))))

(defn triangular-matrix?
  [m]
  (or (every? true?
              (map
               (fn [i row]
                 (if (zero? i)
                   true
                   (every? zero? (take i row))))
               (range (count m)) m))
      (every? true?
              (map
               (fn [i row]
                 (if (= i (dec (count m)))
                   true
                   (every? zero? (take-last (- (dec (count m)) i) row))))
               (range (count m)) m))))

(defn row-adjust
  "using `row-1` to adjust row elements of `row-2` so that their first `n` values are equal to zeros"
  [row-1 row-2 n]
  (if-not (every? zero? (take n row-2))
    (loop [m (range (gu/first-n-zeros row-1) n)
           r-2 row-2
           prev-r-2 row-2]
      (if (< (gu/first-n-zeros r-2) (gu/first-n-zeros prev-r-2))
        prev-r-2
        (if (or (>= (gu/first-n-zeros r-2) n)
                (empty? m))
          r-2
          (let [mth-row-1 (double (nth row-1 (first m)))
                mth-r-2 (double (nth r-2 (first m)))
                mth-row-1-multiplier (Math/abs (/ mth-r-2 mth-row-1))]
            (recur (rest m)
                   (if-not (and (zero? mth-r-2) (zero? mth-row-1))
                     (as-> [row-1] $
                       (perform-arithmetic-op $ (Math/abs mth-row-1-multiplier) *)
                       (perform-arithmetic-op [r-2] $ (if (not= (/ mth-row-1 (Math/abs mth-row-1)) (/ mth-r-2 (Math/abs mth-r-2))) + -))
                       (first $)
                       (map #(if (and (pos? %) (< % 0.0000001)) 0.0 %) $))
                     r-2)
                   r-2)))))
    row-2))

(defn recursive-row-adjust
  [matrix row-index-to-be-processed]
  (loop [row-idxs (range row-index-to-be-processed);(range (first (dimension matrix)))
         result nil]
    (if (or (and (seq result)
                 (or (>= (gu/first-n-zeros result)
                         row-index-to-be-processed)
                     (>= (gu/first-n-zeros (get-val matrix [row-index-to-be-processed]))
                         row-index-to-be-processed)))
            (empty? row-idxs))
      result
      (recur (rest row-idxs)
             (if (or (= (first row-idxs) row-index-to-be-processed)
                     (and (zero? (get-val matrix [(first row-idxs) (dec row-index-to-be-processed)]))
                          (zero? (get-val matrix [(first row-idxs)
                                                  (gu/first-n-zeros
                                                    (get-val matrix [row-index-to-be-processed]))]))))
               result
               (row-adjust (get-val matrix [(first row-idxs)])
                           (or result
                               (get-val matrix [row-index-to-be-processed]))
                           row-index-to-be-processed))))))

(defn upper-triangular-matrix
  "Converts any square matrix into an upper-triangular matrix
   where all the matrix elements below the diagonal elements are zero"
  [matrix]
  (let [[num-rows _] (dimension matrix)]
    (loop [m matrix
           nr (range 1 num-rows)
           swap-count 0]
      (if (empty? nr)
        {:upper-triangular m :num-swaps swap-count}
        (let [adjusted-row (recursive-row-adjust m (first nr))
              first-n-zeros-adjusted-row (gu/first-n-zeros adjusted-row)
              m-adjusted (gu/replace-nth m (first nr) adjusted-row)
              should-be-swapped? (> first-n-zeros-adjusted-row (first nr))
              m-swapped (cond-> m-adjusted
                          should-be-swapped? (swap-rows (first nr) first-n-zeros-adjusted-row))]
          (recur m-swapped
                 (cond-> nr
                   (not should-be-swapped?) rest)
                 (cond-> swap-count
                   should-be-swapped? inc)))))))

(defn determinant
  "Caluclates the determinant of a square matrix by first calculating it's uper triangular matrix
   and then multiplying it's diagonal elements together while taking into account the number of row swaps made in the process"
  [m]
  (let [{:keys [upper-triangular num-swaps]} (upper-triangular-matrix m)]
    (-> (reduce
         (fn [{:keys [i] :as acc} v]
           (update (update acc :result #(* % (nth v (first i)))) :i rest))
         {:i (range (count upper-triangular)) :result 1} upper-triangular)
        :result (* (if (pos? num-swaps) -1.0 1.0)) Math/round)))

(defn cross-product
  "Finds the cross product of two (indexed) rows of a matrix"
  [row-1-indexed row-2-indexed]
  (reduce #(merge-with + %1 %2)
          (map (fn [x]
                 (reduce merge
                         (map
                          (fn [y] {(sort (flatten [(first x) (first y)])) (* (second x) (second y))})
                          row-2-indexed)))
               row-1-indexed)))

(defn eigen-values
  "Gets the eigen values of a matrix"
  [matrix]
  (let [diagonal-elements-minus-lambda (loop [utm (:upper-triangular (upper-triangular-matrix matrix))
                                              i (range (count utm))
                                              result []]
                                         (if (empty? i)
                                           result
                                           (recur (rest utm)
                                                  (rest i)
                                                  (conj result
                                                        [-1 (get-val (first utm) [(first i)])]))))
        eq (vals
             (loop [deml (rest diagonal-elements-minus-lambda)
                    ff (first diagonal-elements-minus-lambda)]
               (if (empty? deml)
                 ff
                 (recur (rest deml)
                        (cross-product (cond-> ff
                                         (not (map? ff)) gu/index-matrix-rows)
                                       (gu/index-matrix-rows (first deml)))))))]
    (lau/solve-equation eq)))
