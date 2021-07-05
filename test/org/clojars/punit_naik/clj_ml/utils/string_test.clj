(ns org.clojars.punit-naik.clj-ml.utils.string-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.utils.string :as su]))

(deftest match-strings-test
  (is (= {:edit-map
          {[4 3] 1
           [2 2] 0
           [0 0] 0
           [1 0] 1
           [2 3] 1
           [3 3] 0
           [1 1] 0
           [3 4] 1
           [4 2] 2
           [3 0] 3
           [4 1] 3
           [1 4] 3
           [1 3] 2
           [0 3] 3
           [2 4] 2
           [0 2] 2
           [2 0] 2
           [0 4] 4
           [3 1] 2
           [2 1] 1
           [4 4] 0
           [1 2] 1
           [3 2] 1
           [0 1] 1
           [4 0] 4}
          :similarity 0}
         (su/match-strings "punit" "punit")))
  (is (= {:edit-map
          {[4 3] 1
           [2 2] 0
           [0 0] 0
           [1 0] 1
           [2 3] 1
           [3 3] 0
           [1 1] 0
           [3 4] 1
           [4 2] 2
           [3 0] 3
           [4 1] 3
           [1 4] 3
           [1 3] 2
           [0 3] 3
           [2 4] 2
           [0 2] 2
           [2 0] 2
           [0 4] 4
           [3 1] 2
           [2 1] 1
           [4 4] 1
           [1 2] 1
           [3 2] 1
           [0 1] 1
           [4 0] 4}
          :similarity 1}
         (su/match-strings "punit" "puniz")))
  (is (= {:edit-map
          {[4 3] 1
           [2 2] 0
           [0 0] 0
           [1 0] 1
           [2 3] 1
           [2 5] 3
           [3 3] 0
           [1 1] 0
           [0 5] 5
           [3 4] 1
           [4 2] 2
           [3 0] 3
           [4 1] 3
           [1 4] 3
           [1 3] 2
           [1 5] 4
           [0 3] 3
           [2 4] 2
           [4 5] 1
           [0 2] 2
           [2 0] 2
           [0 4] 4
           [3 1] 2
           [2 1] 1
           [4 4] 0
           [1 2] 1
           [3 5] 2
           [3 2] 1
           [0 1] 1
           [4 0] 4}
          :similarity 1}
         (su/match-strings "punit" "punitz"))))

(deftest match-char-to-str-test
  (is (= [5 {[0 0] 0, [0 1] 1, [0 2] 2, [0 3] 3, [0 4] 4, [0 5] 5}]
         (su/match-char-to-str {} 0 \p "punitz"))))

(deftest reversed-levenstein-distance-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.utils.string/reversed-levenstein-distance` gives proper output"
    (is (= 100.0 (su/reversed-levenstein-distance "punit" "punit")))
    (is (= 90.0 (su/reversed-levenstein-distance "punit" "puniz")))
    (is (= 90.9090909090909 (su/reversed-levenstein-distance "punit" "punitz")))))