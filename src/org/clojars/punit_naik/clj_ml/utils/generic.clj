(ns org.clojars.punit-naik.clj-ml.utils.generic
  (:require [clojure.string :refer [split]]))

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

(defn replace-nth
  "Replaces `n`th item from `coll` with `replacement`"
  [coll n replacement]
  (concat
   (take n coll)
   (list replacement)
   (drop (inc n) coll)))

(defn rationalise
  "Rationalises a number into a fraction, same as `clojure.core/rationalize`
   But this will always return the numerator as is, without the decimal
   Hence the denominator will be in multiples of 10"
  [n]
  (if (and (not (integer? n))
           (not (zero? (- n (int n)))))
    (let [d (as-> (str n) $
              (split $ #"\.")
              (second $) (count $)
              (take $ (repeat 10))
              (apply * $))]
      (vector (Math/round (* n d)) d))
    [n 1]))

(defn shingles
  "Generate shingles out of a string `s` (could also work with other types of collections)
   The shingle size is specified by `n`
   If `s` is a very small string (of count less than or equal to 5),
   just a list of it's individual chars is returned"
  [s n]
  (if (or (<= (count s) 5)
          (not (re-matches #".*\s+.*" s)))
    (map str s)
    (loop [idx-coll (range (- (count s) (dec n)))
           result []]
      (if (empty? idx-coll)
        result
        (recur (rest idx-coll)
               (conj result (reduce str (take n (drop (first idx-coll) s)))))))))
