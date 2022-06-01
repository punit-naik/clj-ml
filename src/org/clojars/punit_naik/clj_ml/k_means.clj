(ns org.clojars.punit-naik.clj-ml.k-means
  (:require [clojure.set :as set]
            [org.clojars.punit-naik.clj-ml.utils.generic :as generic-utils]
            [org.clojars.punit-naik.clj-ml.utils.geometry :as geometry-utils]
            [org.clojars.punit-naik.clj-ml.utils.matrix :as matrix-utils]))

(defn find-optimal-clusters
  "Finds optimal number of clusters using elbow method"
  [data-points]
  2)

(defn generate-initial-clusters
  [dimensions no-of-clusters]
  (->> (make-array Double/TYPE no-of-clusters dimensions)
       (map-indexed vector)
       (into {})))

(defn assign-closest-cluster
  "Assigns a point to the closest cluster
   Returns the cluster"
  [point clusters-map]
  (->> clusters-map
       (map
        (fn [[index cluster]]
          [index (geometry-utils/distance point cluster)]))
       (into {})
       (apply min-key val)
       val))

(defn update-clusters
  ([data-points]
   (update-clusters data-points (find-optimal-clusters data-points)))
  ([data-points no-of-clusters]
   (update-clusters
    data-points
    (generate-initial-clusters
     (-> data-points first count)
     no-of-clusters)
    0.001))
  ([data-points clusters error-rate]
   (loop [clusterz clusters
          within-error-rate? false]
     (if within-error-rate?
       (vals clusterz)
       (let [inverted-clusterz (set/map-invert clusterz)
             cluster-assignments (reduce
                                  (fn [m point]
                                    (update m (assign-closest-cluster point clusters) conj point))
                                  (->> clusterz
                                       vals
                                       (map #(conj [] % []))
                                       (into {}))
                                  data-points)
             old-new-clusterz (map (fn [[cluster assigned-points]]
                                     [cluster (->> assigned-points
                                                   matrix-utils/transpose
                                                   (map generic-utils/mean-coll))])
                                   cluster-assignments)
             clusterz-error-rates (->> old-new-clusterz
                                       (map
                                        (fn [[old-cluster new-cluster]]
                                          (<= (Math/abs
                                               (- (generic-utils/mean-coll old-cluster)
                                                  (generic-utils/mean-coll new-cluster)))
                                              error-rate))))
             new-clusterz (->> old-new-clusterz
                               (map
                                (fn [[old-cluster new-cluster]]
                                  [(get inverted-clusterz old-cluster)
                                   new-cluster]))
                               (into {}))]
         (recur new-clusterz (every? true? clusterz-error-rates)))))))