(ns graphclj.core-test
  (:require [clojure.test :refer :all]
            [graphclj.core :refer :all]
            [graphclj.centrality :refer :all]
            [graphclj.graph :refer :all]
            [graphclj.tools :refer :all]))
(require '[graphclj.centrality :as C])

(= 0 0)

(deftest test-degrees
  (testing ""
    (is (= (C/degrees {1 {:neigh #{2 3}},0 {:neigh #{1 2}}}) {1 {:neigh #{3 2}, :degree 2}, 0 {:neigh #{1 2}, :degree 2}}))))
