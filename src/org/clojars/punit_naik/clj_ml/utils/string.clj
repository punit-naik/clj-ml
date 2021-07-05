(ns org.clojars.punit-naik.clj-ml.utils.string)

(defn match-char-to-str
  "Matches a characted `c` with all characters of `string`
   Also takes in the position of char `c` i.e. `c-pos` in it's original string
   And an `edit-map` initial value from previous stages"
  [edit-map-init c-pos c string]
  (let [c-pos-zero? (zero? c-pos)]
    (reduce (fn [[edit-distance edm]
                 [string-char-pos string-char]]
              (let [e (if (= c string-char) 0 1)
                    string-char-pos-zero? (zero? string-char-pos)
                    new-edit-distance (min
                                       (if string-char-pos-zero?
                                         (+ c-pos 2)
                                         (inc edit-distance))
                                       (if c-pos-zero?
                                         (+ string-char-pos 2)
                                         (inc (edm [(dec c-pos) string-char-pos])))
                                       (+ (cond
                                            c-pos-zero? string-char-pos
                                            string-char-pos-zero? c-pos
                                            :else (edm [(dec c-pos) (dec string-char-pos)]))
                                          (if (and c-pos-zero? string-char-pos-zero?) 0 e)))]
                [new-edit-distance (assoc edm [c-pos string-char-pos] new-edit-distance)]))
          [(if (zero? c-pos) 0 c-pos) edit-map-init]
          (map-indexed #(conj [] %1 %2) string))))

(defn match-strings
  "Matches two strings `s1` and `s2` and finds out their `edit distance` and `similarity`"
  [s1 s2]
  (reduce (fn [{:keys [edit-map] :as m} [string-char-pos string-char]]
            (let [[edit-distance edit-distance-map] (match-char-to-str edit-map string-char-pos string-char s2)]
              (assoc m :edit-map edit-distance-map :similarity edit-distance)))
          {:edit-map {}} (map-indexed #(conj [] %1 %2) s1)))

(defn reversed-levenstein-distance
  "Finds out the revered levenstein distance (percentage of match)
   between two strings `s1` and `s2` by normalising the edit distance"
  [s1 s2]
  (let [{:keys [similarity]} (match-strings s1 s2)]
    (double (- 100 (* 100 (/ similarity (+ (count s1) (count s2))))))))
