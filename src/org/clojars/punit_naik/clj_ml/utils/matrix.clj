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

(defn square?
  "Checks if a matrix is quare or not"
  [matrix]
  (let [[m n] (dimension matrix)]
    (= m n)))

(defn identity-matrix?
  "Checks if the matrix `m` is a 2-D identity matrix or not"
  [m]
  (let [[p] (dimension m)]
    (and (matrix? m)
         (square? m)
         (every? true?
                 (flatten
                  (map (fn [i]
                         (map (fn [j]
                                (let [ij (get-val m [i j])]
                                  (if (= i j)
                                    (= ij (if (double? ij) 1.0 1))
                                    (zero? ij))))
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
      (throw
       (AssertionError. "Dimensions of matrices are not the same")))))

(defn matrix-multiply
  "Multiplies two matrices of MxN and NxP dimensions
   Calculates the dot product"
  [a b]
  (if (= (second (dimension a)) (first (dimension b)))
    (let [b-transpose (transpose b)]
      (map
       (fn [row-a]
         (map (fn [col-b] (reduce + (map * row-a col-b))) b-transpose)) a))
    (throw
     (AssertionError.
      (str "The number of columns of the first matrix "
           "are not equal to the number of rows of the second matrix")))))

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
                         (map (fn [j] (vector i j))
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

(defn upper-triangular-matrix?
  [m]
  (->> m
       (map-indexed
        (fn [i row]
          (if (zero? i)
            true
            (every? zero? (take i row)))))
       (every? true?)))

(defn lower-triangular-matrix?
  [m]
  (->> m
       (map-indexed
        (fn [i row]
          (if (= i (dec (count m)))
            true
            (every? zero? (take (- (dec (count m)) i) row)))))
       (every? true?)))

(defn triangular-matrix?
  [m]
  (or (upper-triangular-matrix? m)
      (lower-triangular-matrix? m)))

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
                mth-row-1-multiplier (Math/abs (/ mth-r-2 (if (zero? mth-row-1)
                                                            1 mth-row-1)))]
            (recur (rest m)
                   (if-not (and (zero? mth-r-2) (zero? mth-row-1))
                     (as-> [row-1] $
                       (perform-arithmetic-op $ (Math/abs mth-row-1-multiplier) *)
                       (perform-arithmetic-op [r-2] $
                                              (cond
                                                (and (zero? mth-row-1)
                                                     (zero? mth-r-2)) +
                                                (zero? mth-row-1) +
                                                (zero? mth-r-2) +
                                                :else (if (not= (/ mth-row-1 (Math/abs mth-row-1))
                                                                (/ mth-r-2 (Math/abs mth-r-2))) + -)))
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
  (if (triangular-matrix? matrix)
    {:upper-triangular matrix :num-swaps 0}
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
                     should-be-swapped? inc))))))))

(defn determinant
  "Caluclates the determinant of a square matrix by first calculating it's uper triangular matrix
   and then multiplying it's diagonal elements together while taking into account the number of row swaps made in the process"
  [matrix]
  (let [{:keys [upper-triangular num-swaps]} (upper-triangular-matrix matrix)]
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
  "Finds the positive or the negative parts of the characteristic equation
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
                                        ij-mod (if (coll? ij) (index-matrix-rows ij) ij)]
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
                    (if (map? product)
                      (vals product) product)
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
  (let [solve-eq (fn [eq]
                   (->> (remove zero? eq)
                        (lau/solve-equation :newton)
                        (concat (filter zero? (last (partition-by identity eq))))
                        (map (fn [ev]
                               (if (re-find #"\." (str ev))
                                 (gu/round-decimal ev) ev)))
                        sort))
        [m n] (dimension matrix)
        matrix-minus-unkown-lambda-i (matrix-minus-lambda-i matrix)]
    (if (triangular-matrix? matrix)
      (map (fn [i row]
             (nth row i)) (range (count matrix)) matrix)
      (let [concatenated-matrix-minus-lambda-i (concat-matrix-rows matrix-minus-unkown-lambda-i n)
            first-part-product (characteristic-equation-parts concatenated-matrix-minus-lambda-i m)
            second-part-product (characteristic-equation-parts concatenated-matrix-minus-lambda-i m false)
            bigger-coll (if (>= (count first-part-product) (count second-part-product)) first-part-product second-part-product)
            smaller-coll (if (< (count first-part-product) (count second-part-product)) first-part-product second-part-product)
            smaller-coll-padded (concat (make-array Integer/TYPE (- (count bigger-coll)
                                                                    (count smaller-coll))) smaller-coll)
            smaller-coll-padded-negative (map #(* % -1) smaller-coll-padded)
            eq (map + bigger-coll smaller-coll-padded-negative)]
        (solve-eq eq)))))

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

(defn adjust-rref-indices
  "Adusts the indices of the RREF of a matrix by looking at the first <x> zeros of the rows"
  [n rref]
  (as-> (reduce (fn [acc row]
                  (let [index (gu/first-n-zeros row)
                        all-zeros? (every? zero? row)]
                    (cond-> acc
                      (not all-zeros?) (update :indices #(disj % index))
                      (not all-zeros?) (assoc index row))))
                {:indices (into (hash-set) (range n))} rref) $
        (reduce (fn [acc row]
                  (let [all-zeros? (every? zero? row)
                        index (first (acc :indices))]
                    (cond-> acc
                      all-zeros? (update :indices #(disj % index))
                      all-zeros? (assoc index row)))) $ rref)
        (dissoc $ :indices)
        (into (sorted-map) $)
        (vals $)))

(defn eigen-vector-for-lamba
  "Finds the eigenvector for a matrix with a particular eigenvalue"
  ([matrix lambda] (eigen-vector-for-lamba matrix lambda false))
  ([matrix lambda already-calculated?]
   (let [[_ n] (dimension matrix)
         rref-reversed (->> (matrix-minus-lambda-i matrix lambda)
                            reduced-row-echelon-form
                            (adjust-rref-indices n)
                            (map-indexed (fn [i row] [i row])) reverse)
         zero-row-count (count (filter #(every? zero? (second %)) rref-reversed))]
     (loop [rref-r rref-reversed
            default-val-counter 1.0
            result {}]
       (if (empty? rref-r)
         (cond->> (into (sorted-map) result)
           (<= zero-row-count 1) vals
           (> zero-row-count 1) (map (fn [[_ v]] ((if already-calculated? + -) v 1))))
         (let [first-rref-reversed (first rref-r)
               first-rref-reversed-index (first first-rref-reversed)
               first-rref-reversed-row (second first-rref-reversed)
               all-zeros? (every? zero? first-rref-reversed-row)]
           (recur (rest rref-r) (cond-> default-val-counter
                                  all-zeros? inc)
                  (cond-> result
                    all-zeros? (assoc first-rref-reversed-index (cond->> default-val-counter
                                                                  already-calculated? (- 1.0)))
                    (not all-zeros?)
                    (assoc first-rref-reversed-index
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

(defn invertible?
  "Checks if a matrix is invertible or not"
  [matrix]
  (and (square? matrix)
       (not (zero? (determinant matrix)))))

(defn concat-identity-matrix
  "Concatenates identity matrix of same size to the original matrix to generate [A|I]"
  [matrix]
  (map concat matrix (create-identity-matrix (first (dimension matrix)))))

(defn inverse
  [matrix]
  (when (invertible? matrix)
    (let [matrix-cat-i (concat-identity-matrix matrix)
          matrix-cat-i-rref (reduced-row-echelon-form matrix-cat-i)
          [m] (dimension matrix)
          left-part (map #(take m %) matrix-cat-i-rref)
          right-part (map #(drop m %) matrix-cat-i-rref)]
      (when (identity-matrix? left-part)
        right-part))))

(defn rank
  "Finds the rank of a mtrix by counting the number of non zero rows"
  [m]
  (->> m
       (keep #(when-not (every? zero? %) true))
       count))

(defn solve-linear-equation-system
  "Given a set of linear equations where each equation is represented as:
   ax + by + cz + ... = d
   In a matrix for like:
   [[a1 b1 c1 d1]
    [a2 b2 c2 d2]
    .
    .
    [an bn cn dn]]
   Tries to find the solution for every unknown, x, y, z in this case"
  [eq-matrix]
  (let [rref-m (reduced-row-echelon-form eq-matrix)
        a (map butlast rref-m)]
    (when (= (rank rref-m)
             (rank a))
      (reduce
       (fn [solution [i c-row]]
         (let [a-row (butlast c-row)
               b (last c-row)]
           (assoc solution i
                  (->> (if (= i (dec (count eq-matrix)))
                         (/ b (last a-row))
                         (-> (->> (perform-arithmetic-op [(take-last i a-row)] [(take-last i solution)] *)
                                  first
                                  (apply +))
                             (* -1)
                             (+ b)
                             (/ (nth a-row i))))
                       float))))
       (->> (first a)
            count
            (make-array Float/TYPE)
            (into []))
       (->> rref-m
            (map-indexed vector)
            reverse)))))