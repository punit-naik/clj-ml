(ns org.clojars.punit-naik.clj-ml.lsh-test
  (:require [clojure.test :refer [deftest testing is]]
            [org.clojars.punit-naik.clj-ml.lsh :as lsh]))

(defonce ^:private hash-n-times-input-1 ["pu" "un" "ni" "it"])
(defonce ^:private hash-n-times-output-1 '((5021141219716945760 -1766125429625051294 3785936284838743826 -2556519580058936350 4933814831194791719)
                                           (4196085065967877811 -4996603099351797705 445212061751667084 -3987325637413641104 1709939899252595577)
                                           (-4073837946266610328 6957985399799170529 2105580086830957890 -7530869818602416673 4454219994939730485)
                                           (-912190608895152661 2767453070306349719 -832515207850603420 -3178562138392878377 7261036534977689371)))
(defonce ^:private min-hash-output-1 '(-4073837946266610328 -4996603099351797705 -832515207850603420 -7530869818602416673 1709939899252595577))
(defonce ^:private band-hash-output-1 '(8141146812144696309 -269713646976496655 -1579328875242637855))
(defonce ^:private compare-records-input-1 [[0 "punit"] [1 "punitz"] [2 "punitd"]])
(defonce ^:private compare-records-output-1 #{{:original-index 1
                                               :original-data "punitz"
                                               :possible-duplicate-index 2
                                               :possible-duplicate-data "punitd"
                                               :match-percentage 91.66666666666667}
                                              {:original-index 0
                                               :original-data "punit"
                                               :possible-duplicate-index 1
                                               :possible-duplicate-data "punitz"
                                               :match-percentage 90.9090909090909}
                                              {:original-index 0
                                               :original-data "punit"
                                               :possible-duplicate-index 2
                                               :possible-duplicate-data "punitd"
                                               :match-percentage 90.9090909090909}})
(defonce ^:private merge-candidate-list-input-1 [#{[4 "punit4"] [0 "punit"] [1 "punitn"]}
                                             #{[4 "punit4"] [0 "punit"] [1 "punitn"]}])
(defonce ^:private merge-candidate-list-output-1 #{#{[4 "punit4"] [0 "punit"] [1 "punitn"]}})
(defonce ^:private find-possible-duplicates-output-1 '({:original-index 6
                                                        :original-data "test11"
                                                        :possible-duplicate-index 5
                                                        :possible-duplicate-data "test12"
                                                        :match-percentage 91.66666666666667}
                                                       {:original-index 5
                                                        :original-data "test12"
                                                        :possible-duplicate-index 3
                                                        :possible-duplicate-data "test123"
                                                        :match-percentage 92.3076923076923}
                                                       {:original-index 0
                                                        :original-data "punit"
                                                        :possible-duplicate-index 1
                                                        :possible-duplicate-data "punitn"
                                                        :match-percentage 90.9090909090909}
                                                       {:original-index 4
                                                        :original-data "punit4"
                                                        :possible-duplicate-index 0
                                                        :possible-duplicate-data "punit"
                                                        :match-percentage 90.9090909090909}
                                                       {:original-index 0
                                                        :original-data "punit"
                                                        :possible-duplicate-index 2
                                                        :possible-duplicate-data "punir"
                                                        :match-percentage 90.0}
                                                       {:original-index 4
                                                        :original-data "punit4"
                                                        :possible-duplicate-index 1
                                                        :possible-duplicate-data "punitn"
                                                        :match-percentage 91.66666666666667}))

(deftest hash-n-times-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/hash-n-times` works properly or not"
    (is (= (lsh/hash-n-times hash-n-times-input-1 5) hash-n-times-output-1))))

(deftest min-hash-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/min-hash` works properly or not"
    (is (= (lsh/min-hash hash-n-times-output-1) min-hash-output-1))))

(deftest band-hash-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/band-hash` works properly or not"
    (is (= (lsh/band-hash 2 min-hash-output-1) band-hash-output-1))))

(deftest compare-records-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/compare-records` works properly or not"
    (is (= (lsh/compare-records compare-records-input-1) compare-records-output-1))))

(deftest merge-candidates-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/merge-candidates` works properly or not"
    (is (= (lsh/merge-candidates merge-candidate-list-input-1) merge-candidate-list-output-1))))

(deftest merge-candidates-recursive-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/merge-candidates-recursive` works properly or not"
    (is (= (lsh/merge-candidates-recursive merge-candidate-list-input-1) merge-candidate-list-output-1))))

(deftest find-possible-duplicates-test
  (testing "If the function `org.clojars.punit-naik.clj-ml.lsh/find-possible-duplicates` works properly or not"
    (is (= (lsh/find-possible-duplicates 1 5 2 90 ["punit" "punitn" "punir" "test123" "punit4" "test12" "test11"]) find-possible-duplicates-output-1))))
