(ns org.clojars.punit-naik.clj-ml.k-means
  (:require [org.clojars.punit-naik.clj-ml.utils.generic :as generic-utils]
            [org.clojars.punit-naik.clj-ml.utils.geometry :as geometry-utils]
            [org.clojars.punit-naik.clj-ml.utils.matrix :as matrix-utils]))

(declare find-optimal-clusters)

(defn generate-initial-clusters
  [dimensions no-of-clusters]
  (->> (make-array Double/TYPE no-of-clusters dimensions)
       (map-indexed
        (fn [index cluster]
          [index (map identity cluster)]))
       (into {})))

(defn assign-closest-cluster
  "Assigns a point to the closest cluster
   Returns the `key`/index of the cluster"
  [point clusters-map]
  (->> clusters-map
       (map
        (fn [[index cluster]]
          [index (geometry-utils/distance point cluster)]))
       (into {})
       (apply min-key val)
       key))

(defn assign-cluster
  "Assigns a cluster to all data points"
  [data-points clusters]
  (reduce
   (fn [m point]
     (update m (assign-closest-cluster point clusters) conj point))
   (->> clusters
        keys
        (map #(conj [] % []))
        (into {}))
   data-points))

(defn sum-squared-distance
  [data-points cluster]
  (->> data-points
       (map #(Math/pow (geometry-utils/distance cluster %) 2))
       (reduce +)))

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
          cluster-assignments (assign-cluster data-points clusters)
          within-error-rate? false]
     (if within-error-rate?
       (reduce
        (fn [result [cluster-key cluster]]
          (conj result
                (let [assigned-data-points (get cluster-assignments cluster-key)]
                  {:cluster cluster
                   :assigned-data-points assigned-data-points
                   :sum-squared-distance (sum-squared-distance assigned-data-points cluster)})))
        []
        clusterz)
       (let [old-new-clusterz (map (fn [[cluster-key assigned-points]]
                                     [(get clusterz cluster-key)
                                      (if (seq assigned-points)
                                        (->> assigned-points
                                             matrix-utils/transpose
                                             (map generic-utils/mean-coll))
                                        (make-array Double/TYPE (-> data-points first count)))])
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
                                (fn [[old-cluster-key _] [_ new-cluster]]
                                  [old-cluster-key new-cluster])
                                clusterz)
                               (into {}))]
         (recur new-clusterz
                (assign-cluster data-points new-clusterz)
                (every? true? clusterz-error-rates)))))))

(defn rapidly-changing?
  [ssd-mean]
  (let [{ssd-mean-last :sum-squared-distance-mean} (last ssd-mean)
        {ssd-mean-second-last :sum-squared-distance-mean} (-> ssd-mean butlast last)]
    (> (Math/abs (- ssd-mean-last ssd-mean-second-last)) 0.1)))

(defn elbow-method-data
  "Generates data for using elbow method"
  [data-points]
  (loop [ssd-mean []
         c 1]
    (if (and (seq ssd-mean)
             (> (count ssd-mean) 1)
             (not (rapidly-changing? ssd-mean)))
      ssd-mean
      (let [clusters (update-clusters data-points c)]
        (recur (conj ssd-mean
                     {:sum-squared-distance-mean
                      (->> clusters
                           (map :sum-squared-distance)
                           generic-utils/mean-coll)
                      :clusters (map #(select-keys % [:cluster :assigned-data-points]) clusters)})
               (inc c))))))

(comment
  (def p-2 [[0 0]
            [0 1]
            [1 0]
            [1 1]
            [4 4]
            [4 5]
            [5 4]
            [5 5]])
  (elbow-method-data p-2))