(ns clj-ml.utils.io-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clj-ml.utils.io :refer [find-field-names read-file-lines 
                                     row->map xsv-reader
                                     maps->xsv-data write-xsv-from-maps]]
            [clojure.java.io :refer [make-parents delete-file]]
            [clojure.string :as clj-str])
  (:import [java.util UUID]
           [clojure.lang Cons LazySeq]))

(defonce ^:private file-path-read (str "resources/" (UUID/randomUUID)))
(defonce ^:private file-path-write (str "resources/" (UUID/randomUUID)))
(defonce ^:private delimiter #",")
(defonce ^:private field-names [:a :b :c])
(defonce ^:private row-str "1,2,3")
(defonce ^:private xsv-row-data ["a,b,c" "1,2,3"])
(defonce ^:private xsv-row-data-tabs ["a\tb\tc" "1\t2\t3"])
(defonce ^:private xsv-data (clj-str/join "\n" xsv-row-data))
(defonce ^:private xsv-data-after-reading [{:a "1" :b "2" :c "3"}])

(defn temp-file-fixture [my-tests-runner]
  (make-parents file-path-read)
  (spit file-path-read xsv-data)
  (my-tests-runner)
  (delete-file file-path-read)
  (delete-file file-path-write))

(use-fixtures :once temp-file-fixture)

(deftest find-field-names-test
  (testing "Checking if the field names are found determined from the xsv file"
    (is (= (find-field-names file-path-read delimiter) field-names))))

(deftest read-file-lines-test
  (testing "Checking if the output is a lazy sequence"
    (let [rfl (read-file-lines file-path-read)]
      (is (= (class rfl) Cons))
      (is (= (class (rest rfl)) LazySeq)))))

(deftest row->map-test
  (testing "Checking the `row->map` function's successful output"
    (is (= (row->map [row-str] delimiter field-names []) xsv-data-after-reading))))

(deftest xsv-reader-test
  (testing "XSV Reader"
    (let [xsv-after-reading (xsv-reader file-path-read)]
      (is (= xsv-after-reading xsv-data-after-reading)))))

(deftest maps->xsv-data-test
  (testing "Checking if the function correctly converts collection of maps into
            a collection of `x` separated values string
            with `x` separated headers string as it's first element"
    (is (= (maps->xsv-data delimiter xsv-data-after-reading)
           xsv-row-data))))

(deftest write-xsv-from-maps-test
  (testing "Writing XSV data to a file"
    (testing "using comma (`,`) as a separator/delimiter"
      (write-xsv-from-maps file-path-write xsv-data-after-reading)
      (is (= (read-file-lines file-path-write) xsv-row-data))
      (delete-file file-path-write))
    (testing "using tab (`\t`) as a separator/delimiter"
      (write-xsv-from-maps file-path-write xsv-data-after-reading "\t")
      (is (= (read-file-lines file-path-write) xsv-row-data-tabs)))))