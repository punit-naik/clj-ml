(ns org.clojars.punit-naik.clj-ml.utils.string-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.string :as su]))

(deftest levenshtein-distance-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.string/levenshtein-distance` gives proper output"
    (is (= 100.0 (su/levenshtein-distance "punit" "punit")))
    (is (= 90.0 (su/levenshtein-distance "punit" "puniz")))))