(ns org.clojars.punit-naik.clj-ml.utils.geometry-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.clojars.punit-naik.clj-ml.utils.geometry :as geometry-utils]))

(deftest distance-test
  (testing "If the `org.clojars.punit-naik.clj-ml.utils.geometry/distance` fn calculates distance between two points correctly"
    (is (= (geometry-utils/distance [0 0] [1 1]) 1.4142135623730951))
    (is (= (geometry-utils/distance [0 0 0] [1 1 1]) 1.7320508075688772))
    (is (= (geometry-utils/distance [1 2 1] [3 7 6]) 7.3484692283495345))
    (is (= (geometry-utils/distance [12 45 2 1] [34 6 1 3]) 44.83302354291979)))
  (testing "If the `org.clojars.punit-naik.clj-ml.utils.geometry/distance` fn throws error for wrong input"
    (try (geometry-utils/distance [0 0] [1 1 1])
         (catch AssertionError e
           (is (= (.getMessage e)
                  geometry-utils/distance-fn-assertion-error-string))))))