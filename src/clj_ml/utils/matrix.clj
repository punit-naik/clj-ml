(ns clj-ml.utils.matrix
  (:require [clj-ml.utils.generic :as gu]))

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

(defn upper-triangular-matrix?
  [m]
  (every? true?
          (map
           (fn [i row]
             (if (zero? i)
               true
               (every? zero? (take i row))))
           (range (count m)) m)))

(defn row-adjust
  "using `row-1` to adjust row elements of `row-2` so that their first `n` values are equal to zeros"
  [row-1 row-2 n]
  (if-not (every? zero? (take n row-2))
    (loop [m (range n)
           r-2 row-2
           prev-r-2 row-2]
      (if (< (gu/first-n-zeros r-2) (gu/first-n-zeros prev-r-2))
        prev-r-2
        (if (or (>= (gu/first-n-zeros r-2) n)
                (empty? m))
          r-2
          (let [mth-row-1 (double (nth row-1 (first m)))
                mth-r-2 (double (nth r-2 (first m)))
                mth-row-1-multiplier (double (/ mth-r-2 mth-row-1))]
            (recur (rest m)
                   (if-not (zero? mth-r-2)
                     (as-> [row-1] $
                       (perform-arithmetic-op $ (Math/abs mth-row-1-multiplier) *)
                       (perform-arithmetic-op [r-2] $ (if (not= (/ mth-row-1 mth-row-1) (/ mth-r-2 mth-r-2)) + -))
                       (first $))
                     r-2)
                   r-2)))))
    row-2))

(defn upper-triangular-matrix
  "Converts any square matrix into an upper-triangular matrix
   where all the matrix elements below the diagonal elements are zero"
  [matrix]
  (let [sorted-matrix-map (gu/sort-by-first matrix)
        sorted-matrix (-> sorted-matrix-map (dissoc :swap-count) vals)]
    (loop [m sorted-matrix
           prev-m sorted-matrix
           n (range 1 (count m))
           num-swaps (:swap-count sorted-matrix-map)
           comparing-rows (range (first n))]
      (let [nth-row  (nth m (or (first n) 0))
            first-row (nth m (or ((if (pos-int? (gu/first-n-zeros nth-row)) last first) comparing-rows) 0))
            stuck-at-the-same-output? (and (not= sorted-matrix m prev-m) (= prev-m m) (seq n))
            mod-m (cond-> m
                          ;; Swapping row at `(first n)` inside `m` with the first row
                          stuck-at-the-same-output? (gu/replace-nth ((if (pos-int? (gu/first-n-zeros nth-row)) last first) comparing-rows) nth-row)
                          stuck-at-the-same-output? (gu/replace-nth (first n) first-row))
            num-swaps (cond-> num-swaps stuck-at-the-same-output? inc)]
        (if (or (upper-triangular-matrix? mod-m)
                (empty? n))
          {:upper-triangular mod-m :num-swaps num-swaps}
          (let [adjusted-row-nth (row-adjust (nth mod-m ((if (pos-int? (gu/first-n-zeros (nth mod-m (first n)))) last first) comparing-rows))
                                             (nth mod-m (first n)) (first n))
                first-n-zeros-count (gu/first-n-zeros adjusted-row-nth)
                first-n-zeros-count-fixed (cond-> first-n-zeros-count
                                            (= first-n-zeros-count (count mod-m)) dec)
                mod-m-first-n-zeros-count-row (nth mod-m first-n-zeros-count-fixed)
                belongs-to-lower? (> first-n-zeros-count (first n))
                new-n (filter #(not= % first-n-zeros-count-fixed) n)]
            (recur (cond-> (gu/replace-nth mod-m first-n-zeros-count-fixed adjusted-row-nth)
                           (and (> (count mod-m) 3)
                                (> (count n) 1)
                                (> first-n-zeros-count-fixed (first n))) (gu/replace-nth (first n) mod-m-first-n-zeros-count-row))
                   mod-m
                   new-n
                   (cond-> num-swaps belongs-to-lower? inc)
                   (if (= n new-n) (rest comparing-rows) (range (or (first new-n) 0))))))))))

(defn determinant
  "Caluclates the determinant of a square matrix by first calculating it's uper triangular matrix
   and then multiplying it's diagonal elements together while taking into account the number of row swaps made in the process"
  [m]
  (let [{:keys [upper-triangular num-swaps]} (upper-triangular-matrix m)]
    (-> (reduce
         (fn [{:keys [i] :as acc} v]
           (-> (update acc :result #(* % (nth v (first i))))
               (update :i rest)))
         {:i (range (count upper-triangular)) :result 1} upper-triangular)
        :result (* (if (> num-swaps 0) -1.0 1.0)) Math/round)))

(comment
  (row-adjust [1 3 1 4] [3 9 5 15] 2)
  (row-adjust [0 2 1 1] [0 4 2 3] 2)
  (upper-triangular-matrix [[1 0 0] [0 1 0] [0 0 1]])
  (upper-triangular-matrix [[3 -2 5] [6 -4 7] [5 -4 6]])
  (upper-triangular-matrix [[1 3 1 4] [3 9 5 15] [0 2 1 1] [0 4 2 3]])
  (determinant [[3 -2 5] [6 -4 7] [5 -4 6]])
  (determinant [[1 0 0] [0 1 0] [0 0 1]])
  (determinant [[1 3 1 4] [3 9 5 15] [0 2 1 1] [0 4 2 3]]))
