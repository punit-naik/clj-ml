(ns org.clojars.punit-naik.clj-ml.linear-regression
  (:require [org.clojars.punit-naik.clj-ml.utils.matrix :as matrix]))

(defn generate-input-matrix
  "Receives a data matrix like [[x-1a x-1b x-1c...x-1z y-1]... [x-na x-nb x-nc...x-nz y-n]] with features and outputs
   And generates an input matrix like [[1 x-1a x-1b x-1c...x-1z]... [1 x-na x-nb x-nc...x-nz]]"
  [data]
  (map (comp (partial cons 1) drop-last) data))

(defn generate-output-matrix
  "Receives a data matrix like [[x-1a x-1b x-1c...x-1z y-1]... [x-na x-nb x-nc...x-nz y-n]] with features and outputs
   And generates an output matrix like [[y-1]... [y-n]]"
  [data]
  (map (comp list last) data))

(defmulti betas
  "Receives a dispath value `method` and data matrix like [[x-1a x-1b x-1c...x-1z y-1]... [x-na x-nb x-nc...x-nz y-n]] with features and outputs
   And generates betas list like [b-0 b-a..b-z] using linear regression which can be used to predict output for any input [x-a...x-z]
   Like Y = b-0 * 1 + b-a * x-a + .... + b-z * x-z"
  (fn [method _]
    method))

(defmethod betas :matrix
  [_ data]
  (let [input-matrix (generate-input-matrix data)
        input-matrix-transposed (matrix/transpose input-matrix)
        output-matrix (generate-output-matrix data)
        input-matrix-transposed-multiplied-by-input-matrix (matrix/matrix-multiply input-matrix-transposed input-matrix)
        input-matrix-transposed-multiplied-by-output-matrix (matrix/matrix-multiply input-matrix-transposed output-matrix)
        input-matrix-transposed-multiplied-by-input-matrix-inverse (matrix/inverse input-matrix-transposed-multiplied-by-input-matrix)]
    (map first (matrix/matrix-multiply input-matrix-transposed-multiplied-by-input-matrix-inverse input-matrix-transposed-multiplied-by-output-matrix))))

(defn predict
  "Predicts output value for a set of features [x-a...x-z]
   Given the beta values [b-0 b-a...b-z] for the training data"
  [betas inputs]
  (->> (cons 1 inputs)
       (map #(* %1 %2) betas)
       (reduce +)))