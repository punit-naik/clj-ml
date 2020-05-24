(ns org.clojars.punit-naik.clj-ml.pca
  (:require [org.clojars.punit-naik.clj-ml.utils.matrix :as mu]))

(defn principal-components
  "Get's the data as a list of lists where each list inside the bigger list represents a row of data
   Selects the `n` number of principal compoennts"
  [data n]
  (let [feature-data (map butlast data)
        label-data (map last data)
        covar-feature-data (mu/covariance feature-data)
        evals (mu/eigen-values covar-feature-data)
        evecs (mu/eigen-vectors covar-feature-data evals)
        evals-evecs (sort-by first > (map vector evals evecs))]
    (->> (take n evals-evecs)
         (map (fn [[_ evec]]
                (mu/matrix-multiply feature-data (mu/transpose [evec]))))
         (apply map vector)
         (map (fn [label row]
                (conj (vec (flatten row)) label)) label-data))))