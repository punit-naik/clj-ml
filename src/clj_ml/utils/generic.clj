(ns clj-ml.utils.generic)

(defn index-matrix-rows
  "This function indexes `matrix`'s rows and returns a map where key is the row number and value is the row itself"
  [matrix]
  (dissoc
   (reduce
    (fn [{:keys [size] :as acc} v]
      (assoc acc (first size) v :size (rest size)))
    {:size (range (count matrix))} matrix) :size))

(defn first-n-zeros
  "Given a collection, this function finds the number of zero elements of the collection from the start"
  [coll]
  (loop [c coll
         count 0
         last-elem-zero? nil]
    (if (or (false? last-elem-zero?)
            (empty? c))
      count
      (let [lez? (zero? (first c))]
        (recur (rest c) (cond-> count lez? inc) lez?)))))

(defn sort-by-first
  "Sorts the rows of a 2-D matrix by the first element of every row
   while returning the number of swaps made"
  [m]
  (let [matrix (index-matrix-rows m)
        sorted-matrix-temp (sort-by (fn [[f]] f) m)
        sorted-matrix-temp-starting-zeros (->> sorted-matrix-temp
                                               (filter (fn [[f]] (zero? f)))
                                               (sort-by (fn [x] (first-n-zeros x)))
                                               vec)
        sorted-matrix-temp-starting-non-zeros (vec (remove (fn [[f]] (zero? f)) sorted-matrix-temp))
        update-idx (first-n-zeros (first sorted-matrix-temp-starting-zeros))
        sorted-matrix (index-matrix-rows (concat (take update-idx sorted-matrix-temp-starting-non-zeros)
                                                 sorted-matrix-temp-starting-zeros
                                                 (nthrest sorted-matrix-temp-starting-non-zeros update-idx)))]
    (reduce
     (fn [acc k]
       (let [m-k (matrix k)
             sm-k (sorted-matrix k)
             should-be-swapped? (not= m-k sm-k)]
         (cond-> (assoc acc k sm-k)
           should-be-swapped? (update :swap-count inc))))
     {:swap-count 0} (keys matrix))))

(defn replace-nth
  "Replaces `n`th item from `coll` with `replacement`"
  [coll n replacement]
  (concat
   (take n coll)
   (list replacement)
   (drop (inc n) coll)))