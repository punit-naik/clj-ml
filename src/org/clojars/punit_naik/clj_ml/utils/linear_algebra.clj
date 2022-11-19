(ns org.clojars.punit-naik.clj-ml.utils.linear-algebra
  (:require [org.clojars.punit-naik.clj-ml.utils.calculus :as cu]
            [org.clojars.punit-naik.clj-ml.utils.generic :as gu]
            [clojure.set :refer [intersection]]))

(defn eval-fn
  "Evaluates a function ax^n+bx^n-1+...+z represented by a collection of it's coefficients [a b ... z]
   at the value `x`"
  [eq x]
  (reduce + (map-indexed (fn [idx coeff]
                           (* coeff (Math/pow x (- (dec (count eq)) idx)))) eq)))

(defn solve-quadratic-equation
  "Given the a,b and c terms of the quadratic equation ax^2+bx+c=0
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
   (loop [n (range 1 (inc (Math/abs num)))
          result #{}]
     (if (empty? n)
       result
       (recur (rest n)
              (cond-> result
                (zero? (mod (Math/abs num) (first n))) (conj (first n)))))))
  ([num decimal?]
   (if decimal?
     (let [[up down] (gu/rationalise (Math/abs num))]
       (cond-> (factors up)
         (not= down 1) (intersection (factors down))))
     (factors num))))

(defn isa-solution?
  "Given the a to z terms of the quadratic equation ax^n+....+z=0 as a collection
   And the root, this function checks if the same root is a solution for the equation or not
   And returns the new reduced equation for finding the remaining roots using Synthetic Division"
  [coefficients root]
  (let [result (reduce (fn [{:keys [sum] :as acc} v]
                         (let [s (+ (if (zero? sum) v (* root sum)) (if (zero? sum) sum v))]
                           (-> acc
                               (assoc :sum s)
                               (update :coeffs conj s))))
                       {:sum 0 :coeffs []} coefficients)]
    (when (and (zero? (Math/round (* (:sum result) 1.0)))
               (seq (:coeffs result)))
      (:coeffs (update result :coeffs butlast)))))

(defn find-all-possible-solutions
  "Given the a to z terms of the equation ax ^n+....+z=0 as a collection
   This function finds all the possible roots of this equation"
  [coefficients]
  (condp = (count coefficients)
    2 [(/ (* (second coefficients) -1)
          (first coefficients))]
    3 (solve-quadratic-equation {:a (nth coefficients 0)
                                 :b (nth coefficients 1)
                                 :c (nth coefficients 2)})
    (let [first-coefficient (first coefficients)
          last-coefficient (last coefficients)
          first-coeff-factors (sort (factors first-coefficient true))
          last-coeff-factors (sort (factors last-coefficient true))]
      (->> (map (fn [i]
                  (map (fn [j]
                         (if (> (Math/abs last-coefficient)
                                (Math/abs first-coefficient))
                           [(/ j i) (/ (* j -1) i)]
                           [(/ i j) (/ (* i -1) j)]))
                       last-coeff-factors))
                first-coeff-factors)
           flatten distinct sort))))

(defn solve-equation-synthetic-division
  "Given the a to z terms of the equation ax^n+....+z=0
   This returns all the roots for the equation using synthetic division"
  [coefficients]
  (loop [coeffs coefficients
         all-possible (reverse (find-all-possible-solutions coeffs))
         solutions []]
    (if (or (= (count solutions)
               (dec (count coefficients)))
            (empty? all-possible))
      (map #(* %1 1.0) solutions)
      (let [testing-root (first all-possible)
            next-eq (isa-solution? coeffs testing-root)]
        (recur (or next-eq coeffs)
               (if next-eq
                 (reverse (find-all-possible-solutions next-eq))
                 (rest all-possible))
               (cond-> solutions
                 next-eq (conj testing-root)))))))

(defn newtons-method
  "Uses Newton's method to find the root of an equation ax^n+bx^n-1+...+z
   Represented as a collection of it's coefficients [a b ... z]
   It selects a root for precision upto the number set by the arg `precision`"
  [eq eq-deriv precision x-0]
  (loop [testing-root x-0
         error 1]
    (let [f-x (eval-fn eq testing-root)]
      (if (or (zero? f-x)
              (<= error (gu/error-decimal precision)))
      testing-root
      (let [f-dash-x (eval-fn eq-deriv testing-root)
            new-root (if (zero? f-dash-x)
                       ((if (= (/ testing-root (Math/abs testing-root)) -1) + -)
                        testing-root (gu/error-decimal precision))
                       (- testing-root (/ f-x f-dash-x)))]
        (recur new-root (Math/abs (- new-root testing-root))))))))

(defn solve-equation-newtons-method
  "Given the a to z terms of the equation ax^n+....+z=0
   This returns all the roots for the equation using newton's method"
  [coefficients]
  (let [precision 5]
    (->> (find-all-possible-solutions coefficients)
         (map #(gu/approximate-decimal
                (newtons-method coefficients
                                (cu/derivative coefficients)
                                precision %)
                precision))
         distinct)))

(defmulti solve-equation
  "Given the a to z terms of the equation ax^n+....+z=0
   This returns all the roots for the equation"
  (fn [method _] method))

(defmethod solve-equation :synthetic-division
  [_ coefficients]
  (solve-equation-synthetic-division coefficients))

(defmethod solve-equation :newton
  [_ coefficients]
  (solve-equation-newtons-method coefficients))

(defmethod solve-equation :default
  [_ coefficients]
  (let [sesd (solve-equation-synthetic-division coefficients)]
    (if (seq sesd) sesd (solve-equation-newtons-method coefficients))))
