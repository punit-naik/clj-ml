(ns org.clojars.punit-naik.clj-ml.utils.matrix
  (:require [org.clojars.punit-naik.clj-ml.utils.generic :as gu]
            [org.clojars.punit-naik.clj-ml.utils.linear-algebra :as lau]))

(defn equal-dimensions?
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

(defn index-matrix-rows
  "This function indexes `matrix`'s rows and returns a map where key is the row number and value is the row itself"
  [matrix]
  (dissoc
   (reduce
    (fn [{:keys [size] :as acc} v]
      (assoc acc (first size) v :size (rest size)))
    {:size (range (count matrix))} matrix) :size))

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
  ([m _]
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

(defn mean
  "Calculates the mean of a 2-D matrix"
  [m]
  (let [averaged-rows (map (fn [row] (double (/ (reduce + row) (count row)))) m)]
    (gu/mean-coll averaged-rows)))

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
              i-mean (gu/mean-coll value)]
          (recur (rest tm)
                 (concat result
                         (map
                          (fn [x]
                            (let [j (:value (get-val t [x]))
                                  j-mean (gu/mean-coll j)]
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
  (if-not (and (= (dec (count matrix)) row-index-to-be-processed)
               (every? zero? (nth matrix row-index-to-be-processed)))
    (loop [row-idxs (range row-index-to-be-processed)
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
                                                     (get-val matrix [row-index-to-be-processed]))]))
                            (not (nil? result))))
                 result
                 (row-adjust (get-val matrix [(first row-idxs)])
                             (or result
                                 (get-val matrix [row-index-to-be-processed]))
                             row-index-to-be-processed)))))
    (nth matrix row-index-to-be-processed)))

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
              should-be-swapped? (and (> first-n-zeros-adjusted-row (first nr))
                                      (not= first-n-zeros-adjusted-row num-rows))
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
        :result (* (if (pos? num-swaps) -1.0 1.0)) gu/round-decimal)))

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

(defn concat-matrix-rows
  "Concatenates matrix rows with the first `(num-cols - 1)` values of the same row
   This helps in finding the characteristic equation of the matrix"
  [matrix num-cols]
  (map #(concat % (take (dec num-cols) %)) matrix))

(defn characteristic-equation-parts
  "Finds the positive or the negative parts of the characteristic euqation
   Both the positive and negative parts will be added to form the final eqution"
  ([concatenated-matrix-minus-lambda-i num-rows]
   (characteristic-equation-parts concatenated-matrix-minus-lambda-i num-rows true))
  ([concatenated-matrix-minus-lambda-i num-rows positive-part?]
   (let [end-col-idx (dec (second (dimension concatenated-matrix-minus-lambda-i)))
         i-range (if positive-part?
                   (range num-rows)
                   (range end-col-idx (- end-col-idx num-rows) -1))]
     (loop [i i-range
            result []]
       (if (empty? i)
         result
         (let [product (loop [paths (map vector
                                         (range num-rows)
                                         (range (first i)
                                                ((if positive-part? + -) (first i) num-rows)
                                                (if positive-part? 1 -1)))
                              result {}]
                         (if (empty? paths)
                           result
                           (recur (rest paths)
                                  (let [ij (get-val concatenated-matrix-minus-lambda-i (first paths))
                                        ij-mod (if (coll? ij) (gu/index-matrix-rows ij) ij)]
                                    (if (and (coll? result)
                                             (empty? result))
                                      ij-mod
                                      (if (coll? result)
                                        (if (coll? ij-mod)
                                          (cross-product result ij-mod)
                                          (into {} (map (fn [[k v]] [k (* v ij-mod)]) result)))
                                        (if (coll? ij-mod)
                                          (into {} (map (fn [[k v]] [k (* v result)]) ij-mod))
                                          (* ij-mod result))))))))]
           (recur (rest i)
                  (if (and (coll? result)
                           (empty? result))
                    (vals product)
                    (if (coll? result)
                      (if (coll? product)
                        (let [bigger-coll (if (>= (count product) (count result)) (vals product) result)
                              smaller-coll (if (< (count product) (count result)) (vals product) result)
                              smaller-coll-padded (concat (take (- (count bigger-coll) (count smaller-coll)) (repeat 0)) smaller-coll)]
                          (map + bigger-coll smaller-coll-padded))
                        (update (vec result) (dec (count result)) + product))
                      (if (coll? product)
                        (update (vec (vals product)) (dec (count product)) + result)
                        (+ result product)))))))))))

(defn matrix-minus-lambda-i
  "Does A-λI when λ is known as well as unkonwn"
  ([matrix] (matrix-minus-lambda-i matrix nil))
  ([matrix lambda]
   (map #(update (vec %1) %2 (fn [e]
                               (if (nil? lambda)
                                 [-1 e] (- e lambda))))
        matrix (range (first (dimension matrix))))))

(defn eigen-values
  "Gets the eigen values of a matrix from it's characteristic equation"
  [matrix]
  (let [[m n] (dimension matrix)
        matrix-minus-unkown-lambda-i (matrix-minus-lambda-i matrix)
        concatenated-matrix-minus-lambda-i (concat-matrix-rows matrix-minus-unkown-lambda-i n)
        first-part-product (characteristic-equation-parts concatenated-matrix-minus-lambda-i m)
        second-part-product (characteristic-equation-parts concatenated-matrix-minus-lambda-i m false)
        bigger-coll (if (>= (count first-part-product) (count second-part-product)) first-part-product second-part-product)
        smaller-coll (if (< (count first-part-product) (count second-part-product)) first-part-product second-part-product)
        smaller-coll-padded (concat (take (- (count bigger-coll) (count smaller-coll)) (repeat 0)) smaller-coll)
        smaller-coll-padded-negative (map #(* % -1) smaller-coll-padded)
        eq (map + bigger-coll smaller-coll-padded-negative)]
    (->> (remove zero? eq)
         (lau/solve-equation :newton)
         (concat (filter zero? (last (partition-by identity eq))))
         (map (fn [ev]
                (if (re-find #"\." (str ev))
                  (gu/round-decimal ev) ev)))
         sort)))

(defn row-adjust-rref
  "Adjusts the element of `row-1` at index `i` and makes it zero using the elements of `row-2`"
  [row-1 row-2 i]
  (let [row-1-i (nth row-1 i)]
    (if-not (zero? row-1-i)
      (let [row-1-i-abs (Math/abs row-1-i)
            row-2-i (nth row-2 i)
            row-2-i-abs (Math/abs row-2-i)
            row-2-multiplier (/ row-1-i-abs row-2-i-abs)
            minus-or-plus (if (= (double (/ row-1-i row-1-i-abs))
                                 (double (/ row-2-i row-2-i-abs))) - +)]
        (map (fn [row-1-elem row-2-elem]
               (gu/round-decimal
                 (minus-or-plus row-1-elem row-2-elem)))
             row-1 (map #(double (* % row-2-multiplier))
                        row-2)))
      row-1)))

(defn zero-above-below-i-j
  "Makes all the elements above and below `matrix[i, j]` zero using row transformations"
  [matrix [i j] num-rows]
  (let [matrix-i (nth matrix i)]
    (loop [rows-to-be-fixed (concat (range 0 i) (range (inc i) num-rows))
           result matrix]
      (if (empty? rows-to-be-fixed)
        result
        (let [p (first rows-to-be-fixed)
              fixed-row (row-adjust-rref (nth result p) matrix-i j)
              all-zeros? (every? zero? fixed-row)]
          (recur ((if all-zeros? butlast rest) rows-to-be-fixed)
                 (if all-zeros?
                   (concat (remove nil? (gu/replace-nth result p nil)) (list fixed-row))
                   (gu/replace-nth result p fixed-row))))))))

(defn reduced-row-echelon-form
  [matrix]
  (let [[m n] (dimension matrix)
        indexes (range m)
        sorted-matrix (sort-by #(vector (count (filter zero? %))
                                        (gu/first-n-zeros %)) matrix)]
    (loop [idxs indexes
           result nil]
      (if (empty? idxs)
        result
        (recur (rest idxs)
               (let [i (first idxs)
                     row-i (nth (or result sorted-matrix) i)
                     all-zeros? (every? zero? row-i)
                     j (gu/first-n-zeros row-i)
                     ij-th (nth row-i (cond-> j
                                        (= j n) dec))
                     new-result (cond-> (or result sorted-matrix)
                                  (not all-zeros?) (zero-above-below-i-j [i j] m))]
                 (if (and (not all-zeros?)
                          (not= ij-th 1))
                   (gu/replace-nth new-result i (map (fn [e] (double (/ e (cond-> ij-th
                                                                            (zero? ij-th) inc)))) row-i))
                   new-result)))))))

(defn eigen-vector-for-lamba
  "Finds the eigenvector for a matrix with a particular eigenvalue"
  ([matrix lambda] (eigen-vector-for-lamba matrix lambda false))
  ([matrix lambda already-calculated?]
   (let [[_ n] (dimension matrix)
         rref-reversed (->> (matrix-minus-lambda-i matrix lambda)
                            reduced-row-echelon-form
                            (map-indexed (fn [i row] [i row])) reverse)
         zero-row-count (count (filter #(every? zero? (second %)) rref-reversed))]
     (loop [rref-r rref-reversed
            default-val-counter 1.0
            result {}]
       (if (empty? rref-r)
         (cond->> (into (sorted-map) result)
           (<= zero-row-count 1) vals
           (> zero-row-count 1) (map (fn [[_ v]] ((if already-calculated? + -) v 1))))
         (recur (rest rref-r) (inc default-val-counter)
                (let [first-rref-reversed (first rref-r)
                      first-rref-reversed-index (first first-rref-reversed)
                      first-rref-reversed-row (second first-rref-reversed)
                      all-zero? (every? zero? first-rref-reversed-row)]
                  (cond-> result
                    all-zero? (assoc (cond->> first-rref-reversed-index
                                       (let [second-rref-reversed (second rref-r)
                                             second-rref-reversed-index (first second-rref-reversed)
                                             second-rref-reversed-row (second second-rref-reversed)
                                             all-zero-second? (every? zero? second-rref-reversed-row)]
                                         (and (not all-zero-second?)
                                              (not= second-rref-reversed-index
                                                    (gu/first-n-zeros second-rref-reversed-row)))) (- (dec n)))
                                     (cond->> default-val-counter
                                       already-calculated? (- 1.0)))
                    (not all-zero?)
                    (assoc (cond->> first-rref-reversed-index
                             (contains? result first-rref-reversed-index) (- (dec n)))
                           (* -1.0 (reduce + (map
                                              (fn [row-v i] (* row-v (get result i 0)))
                                              first-rref-reversed-row (range n)))))))))))))

(defn eigen-vectors
  "Finds the Eigenvectors of a matrix by using it's Eigenvalues"
  [matrix eigen-values]
  (loop [evals eigen-values
         prev-eval nil
         evecs []]
    (if (empty? evals)
      evecs
      (recur (rest evals)
             (first evals)
             (conj evecs (eigen-vector-for-lamba matrix (first evals) (= prev-eval (first evals))))))))

(comment
  (defn row-echelon-form
    "Calculates the Row Echelon Form (REF) of a matrix"
    [matrix]
    ;(sort-by #(count (filter zero? %)) matrix)
    (sort-by gu/first-n-zeros (:upper-triangular (upper-triangular-matrix matrix))))
  
  (defn pivot-indicies
    "Gets the indices of pivots in each row of an REF matrix"
    [ref-matrix]
    (let [[_ n] (dimension ref-matrix)]
      (map (fn [row]
             (let [pivot-index (gu/first-n-zeros row)]
               (when (< pivot-index n) pivot-index)))
           ref-matrix)))
  
  (defn adjust-element-at-pivot-indices
    "Adjusts all the row elements at the pivot indices and makes them equal to `1`
  By diving all the row elements by the element at the pivot index"
    [ref-matrix]
    (map (fn [row]
           (let [fnz (gu/first-n-zeros row)]
             (if (not= fnz (second (dimension ref-matrix)))
               (let [fnz-row-val (double (nth row fnz))]
                 (if (not= fnz-row-val 1.0)
                   (map #(double (/ % fnz-row-val)) row) row)) row)))
         ref-matrix))
  
  (defn adjust-elements-above-pivot-indices
    "Adjusts all the row elements above pivot indices columns to zero"
    [ref-matrix pivot-indicies]
    (loop [refm (map-indexed vector ref-matrix)
           pi (map-indexed vector pivot-indicies)
           result {}]
      (if (empty? refm)
        result
        (let [first-refm (first refm)
              first-refm-index (first first-refm)
              first-refm-value (second first-refm)]
          (recur (rest refm)
                 (rest pi)
                 (let [intermediate-result (->> (rest pi)
                                                (remove #(nil? (second %)))
                                                reverse
                                                (reduce
                                                 (fn [acc [row-index pivot-index]]
                                                   (merge acc
                                                          (reduce (fn [acc2 v2]
                                                                    (cond-> acc2
                                                                      (not (contains? result v2))
                                                                      (assoc v2
                                                                             (row-adjust-rref (get acc v2 (nth ref-matrix v2))
                                                                                              (get acc row-index (nth ref-matrix row-index))
                                                                                              pivot-index)))) {} (reverse (range row-index))))) {}))]
                   (cond-> result
                     (not (every? zero? first-refm-value))
                     (merge (or (seq intermediate-result)
                                (assoc {} first-refm-index (get result first-refm-index first-refm-value))))

                     (every? zero? first-refm-value)
                     (assoc first-refm-index first-refm-value))))))))
  
  (defn reduced-row-echelon-form-deprecated
    "Calculates the Reduced Row Echelon Form (RREF) of a REF matrix (deprecated)"
    [ref-matrix]
    (->> (adjust-elements-above-pivot-indices (adjust-element-at-pivot-indices ref-matrix)
                                              (pivot-indicies ref-matrix))
         (into (sorted-map)) vals))
  
  (nth [1 2 3] (or nil 0))
  (upper-triangular-matrix [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]])
  (row-echelon-form [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]])
  (pivot-indicies (row-echelon-form [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]]))
  (adjust-element-at-pivot-indices (row-echelon-form [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]]))
  (adjust-elements-above-pivot-indices (adjust-element-at-pivot-indices (row-echelon-form [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]]))
                                       (pivot-indicies (row-echelon-form [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]])))
  (row-adjust-rref [0 1 2 3] [0 0 0 1] 3)
  (reduced-row-echelon-form (row-echelon-form [[2 -2 4 -2] [2 1 10 7] [-4 4 -8 4] [4 -1 14 6]]))
  (reduced-row-echelon-form (matrix-minus-lambda-i [[-5 -6 3] [3 4 -3] [0 0 -2]] -2))
  (reduced-row-echelon-form [[-2 -2 -2] [-2 -5 1] [-2 1 -5]])
  (eigen-values [[-1,2,2],[2,2,-1],[2,-1,2]])
  (matrix-multiply [[-1,2,2],[2,2,-1],[2,-1,2]] [[1.5] [2] [1]])
  (eigen-vectors [[-1,2,2],[2,2,-1],[2,-1,2]] (eigen-values [[-1,2,2],[2,2,-1],[2,-1,2]]))
  (reduced-row-echelon-form (row-echelon-form (matrix-minus-lambda-i [[-16 9 0 0] [12 5 0 0] [0 0 6 -2] [0 0 0 4]] 6)))
  (let [m [[2 1 0] [1 2 1] [0 1 2]]]
    (eigen-vectors m (eigen-values m)))
  (reduced-row-echelon-form '([1 1.4140000000000001 1] [0 1 1.4140000000000001] [1.4140000000000001 1 0]))
  (reduced-row-echelon-form (sort-by #(count (filter zero? %)) (matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 0.586)))
  (reduced-row-echelon-form (row-echelon-form (matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 0.586)))
  (upper-triangular-matrix (matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 0.586))
  (row-adjust [1 0 1] [0 1 0] 1)
  (row-adjust-rref [1.4140000000000001 1 0] [1 1.4140000000000001 1] 0)
  (reduced-row-echelon-form (matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 0.5857864376269049))
  (reduced-row-echelon-form (matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 3.414213562373095))
  (reduced-row-echelon-form (matrix-minus-lambda-i [[2 1 0] [1 2 1] [0 1 2]] 2))
  (eigen-vector-for-lamba [[2 1 0] [1 2 1] [0 1 2]] 0.5857864376269049)
  (let [m [[2 1 0] [1 2 1] [0 1 2]]]
    (eigen-vectors m (eigen-values m)))
  )