(ns org.clojars.punit-naik.clj-ml.utils.calculus)

(defn derivative
  "Finds the derivative of a function ax^n+bx^n-1+...+z represented as a
   Collection of it's coefficients [a b ... z]"
  [eq]
  (map-indexed
   (fn [idx coeff]
     (* coeff (dec (- (count eq) idx))))
   (butlast eq)))
