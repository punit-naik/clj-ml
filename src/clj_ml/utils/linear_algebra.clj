(ns clj-ml.utils.linear-algebra)

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
