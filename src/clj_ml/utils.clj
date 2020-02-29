(ns clj-ml.utils
  (:require [clojure.java.io :as io]
            [clojure.string  :as clj-str])
  (:import [java.io BufferedReader]))

(defn find-field-names
  "Finds the names of the fields of an XSV file"
  [file-path delimiter]
  (map #(-> % clj-str/lower-case keyword)
       (-> (io/reader file-path) BufferedReader. line-seq
           first clj-str/trim-newline (clj-str/split delimiter))))

(defn read-file-lines
  "Reads a file from the `file-path` and returns a lazy sequence of strings"
  [file-path]
  (-> (io/reader file-path) BufferedReader. line-seq))

(defn row->map
  "Takes `rows` as a collection, fetches it's first row,
   Splits it on `delimiter`,
   Coverts the resulting row into a map by using keys from `field-names`,
   And then finally appends it to the `result`"
  [rows delimiter field-names result]
  (as-> rows $$
        (first $$)
        (clj-str/split $$ delimiter)
        (zipmap field-names $$)
        (vector $$)
        (concat result $$)))

(defn xsv-reader
  "Reads an XSV file and generates a collection of maps where each map in the colection
   represent a row with the keys as the column names and it's values as the row's values
   NOTE: X represents a delimiter
   `file-path` is the XSV file's path
   `delimiter` is the delimiter value used in the XSV file, the default value of it is `,`
   `field-names` is the collection of column names of the XSV file
   If it is not supplied, the function will read it from the first row of the file"
  ([file-path] (xsv-reader file-path true))
  ([file-path has-field-names?] (xsv-reader file-path has-field-names? #","))
  ([file-path has-field-names? delimiter]
   (xsv-reader file-path has-field-names? delimiter
               (find-field-names file-path delimiter)))
  ([file-path has-field-names? delimiter field-names]
   (loop [rows (read-file-lines file-path)
          first? has-field-names?
          result (lazy-seq [])]
     (if (empty? rows)
       result
       (recur (rest rows) false
              (cond->> result
                       (not first?) (row->map rows delimiter field-names)))))))
