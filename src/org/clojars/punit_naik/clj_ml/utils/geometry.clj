(ns org.clojars.punit-naik.clj-ml.utils.geometry)

(def distance-fn-assertion-error-string "The number of coordinates/dimensions of the two points are not equal")

(defn distance
  "Calculates distance between two points in space
   A point can be described as a vector/list of [an, bn, cn, .... , zn]
   The distance between two points [a1, b1, c1, .... , z1] and [a2, b2, c2, .... , z2] will be:
   square root of [(a2 - a1)^2 + (b2 - b1)^2 + (c2 - c1)^2 + .... + (z2 - z1)^2]"
  [point-1 point-2]
  (if (= (count point-1)
         (count point-2))
    (->> point-1
         (map
          #(Math/pow (- %1 %2) 2)
          point-2)
         (reduce +)
         Math/sqrt)
    (throw
     (AssertionError. distance-fn-assertion-error-string))))