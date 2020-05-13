(ns org.clojars.punit-naik.clj-ml.utils.generic
  (:require [clojure.string :refer [join split]]))

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

(defn mean-coll
  "Calculates the mean of a collection `c`"
  [c]
  (double (/ (reduce + c) (count c))))

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
  (if (= n 1)
    (map str s)
    (loop [idx-coll (range (- (count s) (dec n)))
           result []]
      (if (empty? idx-coll)
        result
        (recur (rest idx-coll)
               (conj result (reduce str (take n (drop (first idx-coll) s)))))))))

(defn approximate-decimal
  "Given a decimal number `num`, this function approximates/selects it's value upto `n` decimal places."
  [num n]
  (let [[f s] (split (str num) #"\.")]
    (Double/parseDouble (str f "." (join (take n s))))))

(defn error-decimal
  "Given a precision value as an integer, this function returns the corresponding error value"
  [n]
  (Double/parseDouble (str "0." (join (take n (repeat 0))) 1)))

(defn round-decimal
  "Rounds of a decimal based on `precision`"
  [num]
  (let [[l t] (split (str num) #"\.")]
    (if (and (>= (count t) 5) ; Choosing for precision of 5 digits after decimal for now
             (not (zero? (Double/parseDouble t))))
      (let [percentage (* (- 1.0 (Double/parseDouble (str "0." t))) 100.0)
            negative? (re-find #"\-" (str l))]
        (cond-> (Double/parseDouble (last (split (str l) #"\-")))
          (<= percentage 0.2) inc
          (>= percentage 99.9) identity
          negative? (* -1)
          (and (not (<= percentage 0.2))
               (not (>= percentage 99.9))) ((if negative? - +) (Double/parseDouble (str "0." t))))) num)))
