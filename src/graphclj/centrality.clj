(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set]))

(declare degrees)
(declare distance)
(declare dist)
(declare new-actual)
(declare get-ens)
(declare get-nodes)
(declare closeness)
(declare closeness-all)

(defn degrees [g]
  "Calculates the degree centrality for each node"
  (into {} (map (fn [x] {(first x) (merge (second x) {:degree (count (get (second x) :neigh))})}) g)))


(defn distance [g n]
  "Calculate the distances of one node to all the others"
  (let [node (get-nodes g)]
    (let [actual (get-ens (get g n))]
      (loop [node node,actual actual,res {}]
        (if (seq node)
          (recur (rest node) actual (dist g res (first node) actual n 1.0))
          res)))))

(defn dist [g res node actual n cpt]
  (if (= node n)
    (assoc res node 0.0)
    (if (contains? res node)
      ()
      (if (contains? actual node)
        (assoc res node cpt)
        (recur g res node (new-actual g actual) n (inc cpt))))))

(defn new-actual [g actual]
  (loop [actual actual,res #{}]
    (if (seq actual)
      (recur (rest actual) (clojure.set/union res (get (get g (first actual)) :neigh)))
      res)))


(defn get-ens [node]
  (get node :neigh))


(defn get-nodes [g]
  (map (fn [x] (first x)) g))


(defn closeness [g n]
  "Returns the closeness for node n in graph g"
  (let [dist (distance g n)]
    (reduce + (map (fn [x] (if (= (second x) 0.0)
                             (second x)
                             (/ 1.0 (second x))))
                  dist))))


(defn closeness-all [g]
  "Returns the closeness for all nodes in graph g"
  (into {} (map (fn [x] {(first x) (merge (second x) {:close (closeness g (first x))})}) g)))

(let
  [g {1 {:neigh #{0 4 3}},
      0 {:neigh #{1 3}},
      3 {:neigh #{0 1 2}},
      4 {:neigh #{1}},
      2 {:neigh #{3}}}]
  (closeness-all g))
