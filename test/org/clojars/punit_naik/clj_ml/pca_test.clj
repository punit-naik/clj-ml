(ns org.clojars.punit-naik.clj-ml.pca-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.pca :as pca]))

(deftest principal-components-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.pca/principal-components` correctly finds the principal components or not"
    (is (= (pca/principal-components [[1 3 0 1.5] [2 5 0 2.3] [3 6 0 3.2] [4 7 0 4.1] [5 8 0 5] [6 9 0 6.7]] 2)
           '([6.428571428571427 1.0 1.5]
             [11.857142857142854 2.0 2.3]
             [16.285714285714285 3.0 3.2]
             [20.714285714285708 4.0 4.1]
             [25.14285714285714 5.0 5]
             [29.571428571428566 6.0 6.7])))))
