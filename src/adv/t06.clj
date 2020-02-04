(ns adv.t06
  (:require [adv.util :refer [split-lines split parse-int zip]]))

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

(defn solve 
  ([] (solve input "COM"))
  ([edges root] 
    (count-levels (build-graph edges)
                  0
                  root)))

(defn route [edges-map v]
  (->> v
       (iterate edges-map)
       (take-while some?)
       (reverse)))

(defn orbital-diff [edges a b]
  (let [edges-map (into {} (map (comp vec reverse) edges))
        route-a (route edges-map a)
        route-b (route edges-map b)
        common (->> (zip route-a route-b)
                    (take-while (fn [[a b]] (= a b)))
                    (count))]
    (+ (- (count route-a) common 1)
       (- (count route-b) common 1))))

(defn solve2 
  ([edges a b] (orbital-diff edges a b))
  ([] (orbital-diff input "YOU" "SAN")))
