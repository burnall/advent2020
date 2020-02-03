(ns adv.t06
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn parse-edges [s]
  (split s #"\)"))

(def input 
  (->> "data/t06.txt"
       (slurp)
       (split-lines)
       (map parse-edges)))

(defn add-edge-to-graph [graph [from to]]
  (let [connected-to (or (graph from) #{})]
    (assoc graph from (conj connected-to to))))
   
(defn build-graph [edges]
  (reduce add-edge-to-graph
          {}
          edges))
    
(defn count-levels [graph level vertex]
  (->> (graph vertex)
       (map (partial count-levels graph (inc level)))
       (reduce + 0)
       (+ level)))

