(ns clj-ml.utils.io-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clj-ml.utils.io :refer [find-field-names read-file-lines 
                                     row->map xsv-reader]]
            [clojure.java.io :refer [make-parents delete-file]])
  (:import [java.util UUID]
           [clojure.lang Cons LazySeq]))

(defonce ^:private file-path (str "resources/" (UUID/randomUUID)))
(defonce ^:private delimiter #",")
(defonce ^:private field-names [:a :b :c])
(defonce ^:private row-str "1,2,3")
(defonce ^:private xsv-data "a,b,c\n1,2,3")
(defonce ^:private xsv-data-after-reading [{:a "1" :b "2" :c "3"}])

(defn temp-file-fixture [my-tests-runner]
  (make-parents file-path)
  (spit file-path xsv-data)
  (my-tests-runner)
  (delete-file file-path))

(use-fixtures :once temp-file-fixture)

(deftest find-field-names-test
  (testing "Checking if the field names are found determined from the xsv file"
    (is (= (find-field-names file-path delimiter) field-names))))

(deftest read-file-lines-test
  (testing "Checkingif the output is a lazy sequence"
    (let [rfl (read-file-lines file-path)]
      (is (= (class rfl) Cons))
      (is (= (class (rest rfl)) LazySeq)))))

(deftest row->map-test
  (testing "Checking the `row->map` function's successful output"
    (is (= (row->map [row-str] delimiter field-names []) xsv-data-after-reading))))

(deftest xsv-reader-test
  (testing "XSV Reader"
    (let [xsv-after-reading (xsv-reader file-path)]
      (is (= xsv-after-reading xsv-data-after-reading)))))