(ns graphclj.core
  (:require [graphclj.graph :as graph]
            [graphclj.tools :as tools]
            [graphclj.centrality :as central])
  (:gen-class))

(declare toDo)
(declare get-param-distance)
(declare get-param-closeness)
(declare get-param-rank)
(declare get-param-toDot)

(defn -main []
  (println "Entrez [0] pour generer un graph depuis un fichier ou [1] pour le generer aleatoirement")
  (let [x (read-line)]
    (case (Integer/parseInt x)
      0 (let [graph (graph/gen-graph (tools/readfile "test.txt"))]
          (toDo graph))
      1 (do (println "Indiquez le nombre de noeuds")
          (let [n (read-line)]
            (println "Indiquez la proba de lien entre pair")
            (let [p (read-line)]
              (let [graph (graph/erdos-renyi-rnd (Integer/parseInt n) (Float/parseFloat p))]
                (toDo graph)))))
      (do (println "=> Mauvais choix") (recur)))))

(defn toDo [g]
  (println "Que souhaite tu faire avec\n"g"\n[0] Degrees\n[1] Distance\n[2] Closeness\n[3] Closeness-all\n[4] Rank-node\n[5] To dot\n[6] Exit")
  (let [x (read-line)]
    (case (Integer/parseInt x)
      0 (do (println "=====Degree du graph=====\n"(central/degrees g)"\n==========================\n") (recur g))
      1 (do (println "=====Distance des noeuds=====\n"(get-param-distance g)"\n============================\n") (recur g))
      2 (do (println "=====Closeness du noeud=====\n"(get-param-closeness g)"\n===========================\n") (recur g))
      3 (do (println "=====Closeness All=====\n"(central/closeness-all g)"\n========================\n") (recur g))
      4 (do (println "=====Rank du graph=====\n"(get-param-rank g)"\n======================\n") (recur g))
      5 (do (println "=====ToDot du graph=====\n"(get-param-toDot g)"\n======================\n") (recur g))
      6 ()
      (do (println "=> Mauvais choix") (recur g)))))


(defn get-param-distance [g]
  (println "Entrez le noeud pour lequel vous voulez les distances")
  (let [x (read-line)]
    (central/distance g (Integer/parseInt x))))

(defn get-param-closeness [g]
  (println "Entrez le noeud pour lequel vous voulez la centralité")
  (let [x (read-line)]
    (central/closeness g (Integer/parseInt x))))

(defn get-param-rank [g]
  (println "Entrez le label pour le rank: close ou degree")
  (let [x (read-line)]
    (case (keyword x)
      :close (tools/rank-nodes (central/closeness-all g) :close)
      :degree (tools/rank-nodes (central/degrees g) :degree)
      (do (println "=> Mauvais choix") (recur g)))))

(defn get-param-toDot [g]
  (println "Entrez le label pour le toDot: close ou degree")
  (let [x (read-line)]
    (case (keyword x)
      :close (tools/to-dot (central/closeness-all g))
      :degree (tools/to-dot (central/degrees g))
      (do (println "=> Mauvais choix") (recur g)))))
