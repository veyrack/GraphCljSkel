(ns graphclj.tools
  (:require [clojure.string :as str]))

(declare readfile)
(declare rank-nodes)
(declare get-rank)
(declare generate-colors)
(declare to-dot)
(declare print-node)
(declare print-link)
(declare get-all-link)
(declare get-link)
(declare delete-doublon)
(declare contains-str?)

(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))

(defn rank-nodes [g,l]
  "Ranks the nodes of the graph in relation to label l in accending order"
  (into {} (map (fn [x] {(first x) (merge (second x) {:rank (get-rank g l (get (second x) l))})}) g)))


(defn get-rank [g l node]
  (loop [s g,res 0]
    (if (seq s)
      (if (< (get (second (first s)) l) node)
        (recur (rest s) (inc res))
        (recur (rest s) res))
      res)))


(defn generate-colors [n]
    (let [step 10]
     (loop [colors {}, current [255.0 160.0 122.0], c 0]
       (if (= c (inc n))
         colors
         (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
                (map #(mod (+ step %) 255) current) (inc c))))))




(defn to-dot [g]
  "Returns a string in dot format for graph g, each node is colored in relation to its ranking"
  (let [colors (generate-colors (count g))
        node (print-node g colors)
        link (print-link g)
        res (str "graph g{\n"node link"}")]
    res))
    ;;Print + return the string
    ;(do (println res) res)))

;(let
;  [g {1 {:neigh #{0 4 3}, :close 4.0},
;      0 {:neigh #{1 3}, :close 3.5 },
;      3 {:neigh #{0 1 2}, :close 4.0},
;      4 {:neigh #{1}, :close 2.8},
;      2 {:neigh #{3}, :close 2.8}))
;  (to-dot g))


(defn print-node [g colors]
  (loop [s g,res ""]
    (if (seq s)
      (recur (rest s) (str res (first (first s))" [style=filled color="(pr-str (str/join " " (get colors (first (first s)))))"]\n"))
      res)))


(defn print-link [g]
  (let [link (delete-doublon (get-all-link g))]
    (loop [s link,res ""]
      (if (seq s)
        (recur (rest s) (str res (first s)"\n"))
        res))))


(defn get-all-link [g]
  (loop [s g,res ""]
    (if (seq s)
      (recur (rest s) (str res (get-link (first (first s)) (get (second (first s)) :neigh))))
      (str/split res #" "))))

(defn get-link [node link]
  (loop [s link,res ""]
    (if (seq s)
      (recur (rest s) (str res (str node"--" (first s)" ")))
      res)))


(defn delete-doublon [entry]
  (loop [s entry,res entry]
    (if (seq s)
      (let [s1 (first (str/split (first s) #"--")) s2 (second (str/split (first s) #"--")) ss (str s2"--"s1)]
        (if (contains-str? res ss)
          (recur (rest s) (remove #{(first s)} res))
          (recur (rest s) res)))
      res)))


(defn contains-str? [vec str]
  (loop [s vec]
    (if (seq s)
      (if (= (first s) str)
        true
        (recur (rest s)))
      false)))
