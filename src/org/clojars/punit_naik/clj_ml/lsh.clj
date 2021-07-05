(ns org.clojars.punit-naik.clj-ml.lsh
  (:require [clojure.set :refer [subset?]]
            [clojure.string :refer [join]]
            [org.clojars.punit-naik.clj-ml.utils.generic :refer [shingles]]
            [org.clojars.punit-naik.clj-ml.utils.string :refer [reversed-levenstein-distance]]
            [æsahættr :refer [murmur3-128 hash->long hash-string]]))

(defn hash-n-times
  "Hashes a shingles list `n` times"
  [sh-list n]
  (let [hashes (map murmur3-128 (range n))]
    (map
     (fn [s] (map (fn [h] (hash->long (hash-string h s))) hashes))
     sh-list)))

(defn min-hash
  "Takes the lists of hashed values (where all of them have the same size)
   and finds the minimum hash value at the position ‘i’ from every list
   thereby generating a single list of hash values which is the minhash signature of that string"
  [hash-values]
  (reduce #(map min %1 %2) hash-values))

(defn band-hash
  "Takes the minhash signature of a string and partitions it according to `band-size`
   Then we hash each \"band\" (partition) as similar strings will tend have at least one matching hashed band"
  [band-size minhash-list]
  (let [banded-minhash-list (partition-all band-size minhash-list)]
    (map
     (fn [m h] (hash->long (hash-string h (join "-" m))))
     banded-minhash-list
     (map murmur3-128 (range (count banded-minhash-list))))))

;(defn find-candidate-subsets
;  "Given a list of tuples of (<str>,<hash>), tries to generate a a list of similar candidates"
;  [str-hash-pair-list]
;  (reduce (fn []) {} str-hash-pair-list))

(defn compare-records
  "Compares a list of records/string with each other using `org.clojars.punit-naik.clj-ml.utils.stringequation/reversed-levenstein-distance`"
  [records]
  (loop [[s1-idx s1] (first records)
         s2-rest (rest records)
         result #{}]
    (if (empty? s2-rest)
      result
      (recur (first s2-rest)
             (rest s2-rest)
             (into result
                   (map
                    (fn [[s2-idx s2]]
                      {:original-index s1-idx
                       :original-data s1
                       :possible-duplicate-index s2-idx
                       :possible-duplicate-data s2
                       :match-percentage (reversed-levenstein-distance s1 s2)}) s2-rest))))))

(defn merge-candidates
  [candidate-list]
  (loop [c (first candidate-list)
         cr (rest candidate-list)
         result #{}]
    (if (empty? cr)
      result
      (recur (first cr)
             (rest cr)
             (into result (filter #(subset? c %) cr))))))

(defn merge-candidates-recursive
  [candidate-list]
  (loop [current-result candidate-list
         merged-result (merge-candidates current-result)]
    (if (or (empty? merged-result)
            (= (count merged-result) (count current-result)))
      current-result
      (recur merged-result (merge-candidates merged-result)))))

(defn find-possible-duplicates
  "Takes a collection of strings (`data`) and finds out the similar strings from the collection"
  [shingle-size hash-count band-size match-threshold data]
  (->> (pmap #(-> %
                  (shingles shingle-size)
                  (hash-n-times hash-count)
                  min-hash) data)
       (mapcat #(map (fn [bh] [(:index %1) (:data %1) bh]) (band-hash band-size %2))
               (map-indexed (fn [idx d] {:index idx :data d}) data))
       (group-by last)
       ;; Not really interested in the key as it is present in the grouped data
       vals
       (pmap #(set (map butlast %)))
       ;; Merging candidates as there might be multiple hash values that match for different strings
       merge-candidates-recursive
       ;; Generating candidates list
       (pmap compare-records)
       (apply concat)
       (filter #(>= (:match-percentage %) match-threshold))))
