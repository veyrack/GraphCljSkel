(ns graphclj.graph
  (:require [clojure.string :as str]))

(declare add-graph)
(declare gen-graph)
(declare transfo-pair)
(declare erdos-renyi-rnd)

(defn add-graph [graph edge]
  (let [[s1 s2] (map #(Integer/parseInt %) (str/split edge #" "))]
    (let [info (if (contains? graph s1)
                 {:neigh (conj (:neigh (get graph s1)) s2)}
                 {:neigh #{s2}})]
      (assoc graph s1 info))))


;; Generate a graph from the lines
(defn gen-graph [lines]
    "Returns a hashmap contating the graph"
    (let [inverse (map (fn [x] (apply str (reverse x))) lines)]
      (reduce add-graph {} (concat lines inverse))))


(defn transfo-pair [coll]
  (if (not (empty? coll))
    (concat
      (map (fn [x] #{(first coll) x}) (rest coll))
      (transfo-pair (rest coll)))
    ()))

(defn erdos-renyi-rnd [n,p]
  "Returns a G_{n,p} random graph, also known as an Erdős-Rényi graph"
  (let [liste (take n (range))
        pair (transfo-pair liste)
        lines (filter (fn [x] (< (rand) p)) pair)
        lines (mapcat (fn [x] [(str (first x) " " (second x))]) lines)]
    (gen-graph lines)))
