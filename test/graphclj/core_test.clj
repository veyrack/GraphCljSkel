(ns graphclj.core-test
  (:require [clojure.test :refer :all]
            [graphclj.core :refer :all]
            [graphclj.centrality :refer :all]
            [graphclj.graph :refer :all]
            [graphclj.tools :refer :all]))


(= 0 0)

(deftest test-centrality
  (testing ""
    (is (= (degrees {1 {:neigh #{2 3}},0 {:neigh #{1 2}}}) {1 {:neigh #{3 2}, :degree 2}, 0 {:neigh #{1 2}, :degree 2}}))
    (let [g {1 {:neigh #{0 4 3}},
             0 {:neigh #{1 3}},
             3 {:neigh #{0 1 2}},
             4 {:neigh #{1}},
             2 {:neigh #{3}}}]
      (is (= (distance g 1) {1 0.0, 0 1.0, 3 1.0, 4 1.0, 2 2.0}))
      (is (= (get-ens {:neigh #{0 4 3}}) #{0 4 3}))
      (is (= (get-nodes g) '(1 0 3 4 2)))
      (is (= (closeness g 0) 3.0))
      (is (= (closeness-all g) {1 {:neigh #{0 4 3}, :close 3.5}, 0 {:neigh #{1 3}, :close 3.0}, 3 {:neigh #{0 1 2}, :close 3.5}, 4 {:neigh #{1}, :close 2.3333333333333335}, 2 {:neigh #{3}, :close 2.3333333333333335}})))))

(deftest test-tools
  (testing "")
  (let
    [g {1 {:neigh #{0 4 3}, :close 4.0},
        0 {:neigh #{1 3}, :close 3.5 },
        3 {:neigh #{0 1 2}, :close 4.0},
        4 {:neigh #{1}, :close 2.8},
        2 {:neigh #{3}, :close 2.8}}]
    (is (= (rank-nodes g :close) {1 {:neigh #{0 4 3}, :close 4.0, :rank 3},
                                  0 {:neigh #{1 3}, :close 3.5, :rank 2},
                                  3 {:neigh #{0 1 2}, :close 4.0, :rank 3},
                                  4 {:neigh #{1}, :close 2.8, :rank 0},
                                  2 {:neigh #{3}, :close 2.8, :rank 0}}))
    (is (= (print-node g (generate-colors 5)) "1 [style=filled color=\"0.0784313725490196 0.7058823529411765 0.5568627450980392\"]\n0 [style=filled color=\"0.0392156862745098 0.6666666666666666 0.5176470588235295\"]\n3 [style=filled color=\"0.1568627450980392 0.7843137254901961 0.6352941176470588\"]\n4 [style=filled color=\"0.19607843137254902 0.8235294117647058 0.6745098039215687\"]\n2 [style=filled color=\"0.11764705882352941 0.7450980392156863 0.596078431372549\"]\n"))
    (is (= (print-link g) "0--1\n3--0\n3--1\n4--1\n2--3\n"))
    (is (= (get-link 1 #{0 3 4}) "1--0 1--4 1--3 "))
    (is (= (get-all-link g) ["1--0" "1--4" "1--3" "0--1" "0--3" "3--0" "3--1" "3--2" "4--1" "2--3"]))
    (is (= (delete-doublon ["1--0" "1--4" "1--3" "0--1" "0--3" "3--0" "3--1" "3--2" "4--1" "2--3"]) '("0--1" "3--0" "3--1" "4--1" "2--3")))
    (is (= (contains-str? ["1--0" "0--1"] "1--0") true))))

(deftest test-graphclj
  (testing "")
  (is (= (add-graph {1 {:neigh #{2}}} "2 1") {1 {:neigh #{2}}, 2 {:neigh #{1}}}))
  (is (= (gen-graph '("1 2" "2 1" "3 1")) {1 {:neigh #{3 2}}, 2 {:neigh #{1}}, 3 {:neigh #{1}}})))
  ;(is (= (erdos-renyi-rnd 5 0.5)))) TESTING ALEA 
