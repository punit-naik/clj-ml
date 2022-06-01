(ns org.clojars.punit-naik.clj-ml.k-means-test
  (:require [clojure.test :refer [deftest is]]
            [org.clojars.punit-naik.clj-ml.k-means :as k-means]))

(defonce sample-data-1
  [[0 0]
   [1 1]
   [1 0]
   [0 1]
   [4 4]
   [4 5]
   [5 4]
   [5 5]])

(defonce sample-data-2
  [[0 0]
   [1 1]
   [1 0]
   [0 1]
   [4 4]
   [4 5]
   [5 4]
   [5 5]
   [10 10]
   [10 11]
   [11 10]
   [11 11]])

(deftest generate-initial-clusters-test
  (is (= (k-means/generate-initial-clusters 2 2)
         {0 [0.0 0.0]
          1 [0.0 0.0]}))
  (is (= (k-means/generate-initial-clusters 2 3)
         {0 [0.0 0.0]
          1 [0.0 0.0]
          2 [0.0 0.0]}))
  (is (= (k-means/generate-initial-clusters 3 2)
         {0 [0.0 0.0 0.0]
          1 [0.0 0.0 0.0]})))

(deftest assign-closest-cluster-test
  (is (= (k-means/assign-closest-cluster
          [1 1]
          {0 [1 1]
           1 [1 0]
           2 [0 1]})
         0))
  (is (= (k-means/assign-closest-cluster
          [3 4.5]
          {0 [1 1]
           1 [4 3]
           2 [0 1]})
         1)))

(deftest assign-cluster-test
  (is (= (k-means/assign-cluster
          sample-data-1
          {0 [0.5 0.5]
           1 [4.5 4.5]})
         {0 [[0 0]
             [1 1]
             [1 0]
             [0 1]]
          1 [[4 4]
             [4 5]
             [5 4]
             [5 5]]}))
  (is (= (k-means/assign-cluster
          sample-data-1
          {0 [0.5 0.5]
           1 [4.5 4.5]
           2 [9 9]})
         {0 [[0 0]
             [1 1]
             [1 0]
             [0 1]]
          1 [[4 4]
             [4 5]
             [5 4]
             [5 5]]
          2 []}))
  (is (= (k-means/assign-cluster
          sample-data-2
          {0 [0.5 0.5]
           1 [4.5 4.5]
           2 [10.5 10.5]})
         {0 [[0 0]
             [1 1]
             [1 0]
             [0 1]]
          1 [[4 4]
             [4 5]
             [5 4]
             [5 5]]
          2 [[10 10]
             [10 11]
             [11 10]
             [11 11]]})))

(deftest sum-squared-distance-test
  (is (= (k-means/sum-squared-distance sample-data-1 [3 3]) 72.0))
  (is (= (k-means/sum-squared-distance sample-data-2 [3 3]) 524.0)))

(deftest update-clusters-test
  (is (= (k-means/update-clusters sample-data-1 2)
         [{:cluster [0.5 0.5]
           :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]
           :sum-squared-distance 2.0000000000000004}
          {:cluster [4.5 4.5]
           :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]
           :sum-squared-distance 2.0000000000000004}]))
  (is (= (k-means/update-clusters sample-data-2 2)
         [{:cluster [0.5 0.5]
           :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]
           :sum-squared-distance 2.0000000000000004}
          {:cluster [7.5 7.5]
           :assigned-data-points [[4 4] [4 5] [5 4] [5 5] [10 10] [10 11] [11 10] [11 11]]
           :sum-squared-distance 148.0}]))
  (is (= (k-means/update-clusters sample-data-2 3)
         [{:cluster [0.5 0.5]
           :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]
           :sum-squared-distance 2.0000000000000004}
          {:cluster [4.5 4.5]
           :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]
           :sum-squared-distance 2.0000000000000004}
          {:cluster [10.5 10.5]
           :assigned-data-points [[10 10] [10 11] [11 10] [11 11]]
           :sum-squared-distance 2.0000000000000004}])))

(deftest rapidly-changing?-test
  (is (not (k-means/rapidly-changing? [{:sum-squared-distance-mean 1.1} {:sum-squared-distance-mean 1.2}])))
  (is (k-means/rapidly-changing? [{:sum-squared-distance-mean 1.1} {:sum-squared-distance-mean 1.6}])))

(deftest elbow-method-data-test
  (is (= (k-means/elbow-method-data sample-data-1)
         [{:sum-squared-distance-mean 68.0
           :distance-from-origin 68.00735254367721
           :clusters
           [{:cluster [2.5 2.5]
             :assigned-data-points [[0 0] [1 1] [1 0] [0 1] [4 4] [4 5] [5 4] [5 5]]}]}
          {:sum-squared-distance-mean 2.0000000000000004
           :distance-from-origin 2.8284271247461903
           :clusters
           [{:cluster [0.5 0.5]
             :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]}
            {:cluster [4.5 4.5]
             :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]}]}
          {:sum-squared-distance-mean 1.1111111111111114,
           :distance-from-origin 3.1991511219751043,
           :clusters
           [{:cluster [0.0 0.0]
             :assigned-data-points [[0 0]]}
            {:cluster [0.6666666666666667 0.6666666666666667]
             :assigned-data-points [[1 1] [1 0] [0 1]]}
            {:cluster [4.5 4.5]
             :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]}]}]))
  (is (= (k-means/elbow-method-data sample-data-2)
         [{:sum-squared-distance-mean 411.33333333333326
           :distance-from-origin 411.33454889069435
           :clusters
           [{:cluster [5.166666666666667 5.166666666666667]
             :assigned-data-points [[0 0] [1 1] [1 0] [0 1] [4 4] [4 5] [5 4] [5 5] [10 10] [10 11] [11 10] [11 11]]}]}
          {:sum-squared-distance-mean 75.0
           :distance-from-origin 75.02666192761077
           :clusters
           [{:cluster [0.5 0.5]
             :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]}
            {:cluster [7.5 7.5]
             :assigned-data-points [[4 4] [4 5] [5 4] [5 5] [10 10] [10 11] [11 10] [11 11]]}]}
          {:sum-squared-distance-mean 2.0000000000000004
           :distance-from-origin 3.6055512754639896
           :clusters
           [{:cluster [0.5 0.5]
             :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]}
            {:cluster [4.5 4.5]
             :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]}
            {:cluster [10.5 10.5]
             :assigned-data-points [[10 10] [10 11] [11 10] [11 11]]}]}
          {:sum-squared-distance-mean 1.3333333333333335
           :distance-from-origin 4.216370213557839
           :clusters
           [{:cluster [0.0 0.0]
             :assigned-data-points [[0 0]]}
            {:cluster [0.6666666666666667 0.6666666666666667]
             :assigned-data-points [[1 1] [1 0] [0 1]]}
            {:cluster [4.5 4.5]
             :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]}
            {:cluster [10.5 10.5]
             :assigned-data-points [[10 10] [10 11] [11 10] [11 11]]}]}])))

(deftest elbow-test
  (is (= (-> sample-data-1
             k-means/elbow-method-data
             k-means/elbow)
         [{:cluster [0.5 0.5]
           :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]}
          {:cluster [4.5 4.5]
           :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]}]))
  (is (= (-> sample-data-2
             k-means/elbow-method-data
             k-means/elbow)
         [{:cluster [0.5 0.5]
           :assigned-data-points [[0 0] [1 1] [1 0] [0 1]]}
          {:cluster [4.5 4.5]
           :assigned-data-points [[4 4] [4 5] [5 4] [5 5]]}
          {:cluster [10.5 10.5]
           :assigned-data-points [[10 10] [10 11] [11 10] [11 11]]}])))

(deftest data-with-assigned-clusters-test
  (is (= (-> sample-data-1
             k-means/elbow-method-data
             k-means/elbow
             k-means/data-with-assigned-clusters)
         [{:data-point [0 0] :cluster [0.5 0.5]}
          {:data-point [1 1] :cluster [0.5 0.5]}
          {:data-point [1 0] :cluster [0.5 0.5]}
          {:data-point [0 1] :cluster [0.5 0.5]}
          {:data-point [4 4] :cluster [4.5 4.5]}
          {:data-point [4 5] :cluster [4.5 4.5]}
          {:data-point [5 4] :cluster [4.5 4.5]}
          {:data-point [5 5] :cluster [4.5 4.5]}]))
  (is (= (-> sample-data-2
             k-means/elbow-method-data
             k-means/elbow
             k-means/data-with-assigned-clusters)
         [{:data-point [0 0] :cluster [0.5 0.5]}
          {:data-point [1 1] :cluster [0.5 0.5]}
          {:data-point [1 0] :cluster [0.5 0.5]}
          {:data-point [0 1] :cluster [0.5 0.5]}
          {:data-point [4 4] :cluster [4.5 4.5]}
          {:data-point [4 5] :cluster [4.5 4.5]}
          {:data-point [5 4] :cluster [4.5 4.5]}
          {:data-point [5 5] :cluster [4.5 4.5]}
          {:data-point [10 10] :cluster [10.5 10.5]}
          {:data-point [10 11] :cluster [10.5 10.5]}
          {:data-point [11 10] :cluster [10.5 10.5]}
          {:data-point [11 11] :cluster [10.5 10.5]}])))