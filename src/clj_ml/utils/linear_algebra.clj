(ns clj-ml.utils.linear-algebra
  (:require [clj-ml.utils.generic :as gu]
            [clojure.set :refer [intersection]]))

(defn solve-quadratic-eq
  "Given the a,b and c terms of the quadratic euqation ax^2+bx+c=0
   This returns a pair of solutions for x"
  [{:keys [a b c]}]
  (let [four-a-c (* 4 a c)
        b-squared (* b b)
        square-root-b-squared-minus-four-a-c (Math/sqrt (- b-squared four-a-c))
        two-a (* 2 a)]
    [(double (/ (+ (* b -1) square-root-b-squared-minus-four-a-c) two-a))
     (double (/ (+ (* b -1) (* square-root-b-squared-minus-four-a-c -1)) two-a))]))

(defn factors
  "Finds all the factors of a number"
  ([num]
   (loop [n (range 1 (inc num))
          result #{}]
     (if (empty? n)
       result
       (recur (rest n)
              (cond-> result
                (zero? (mod num (first n))) (conj (first n)))))))
  ([num decimal?]
   (if decimal?
     (let [[up down] (gu/rationalise num)]
       (intersection (factors up)
                     (factors down)))
     (factors num))))