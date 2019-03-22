(ns graphclj.graph
  (:require [clojure.string :as str]))

;; Generate a graph from the lines
(defn gen-graph [lines]
    "Returns a hashmap contating the graph"
    (loop [s lines,res {}]
      (let [f (first (first s)),r (rest (first s))]
        (if (contains? res f)
          (assoc res f (clojure.set/union (get res f) r))))))




(defn erdos-renyi-rnd [n,p]
  "Returns a G_{n,p} random graph, also known as an Erdős-Rényi graph")
