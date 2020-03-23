(ns org.clojars.punit-naik.clj-ml.utils.string)

(defn levenshtein-distance
  "Finds the percentage of match (reversed levenshtein distance & normalised)
   between two strings"
  [s1 s2]
  (let [similarity (atom 0)
        edit-map (atom {})]
    (doseq [i (range (count s1))]
      (let [edit (atom (if (zero? i) 0 i))]
        (doseq [j (range (count s2))]
          (let [e (if (= (.charAt s1 i) (.charAt s2 j)) 0 1)]
            (reset! edit
                    (min
                     (if (zero? j)
                       (+ i 2)
                       (inc @edit))
                     (if (zero? i)
                       (+ j 2)
                       (inc (@edit-map (str (dec i) "," j))))
                     (+ (cond
                          (zero? i) j
                          (zero? j) i
                          :else (@edit-map (str (dec i) "," (dec j))))
                        (if (and (zero? i) (zero? j)) 0 e))))
            (swap! edit-map assoc (str i "," j) @edit)
            (when (and (= i (dec (count s1))) (= j (dec (count s2))))
              (reset! edit-map nil)
              (reset! similarity @edit))))))
    (double (- 100 (* 100 (/ @similarity (+ (count s1) (count s2))))))))
