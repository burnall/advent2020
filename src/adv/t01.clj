(ns adv.t01
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)
       (map parse-int)))

(defn fuel-mass [mass]
  (-> mass
      (quot 3)
      (- 2)
      ((fn [n] (if (> n 0) n 0)))))  

(defn fuel-mass-all [n]
  (->> n
       (iterate fuel-mass)
       (take-while (comp not zero?))
       (drop 1)))

(defn solve 
  ([modules] (reduce + (map fuel-mass modules)))
  ([] (solve input))) 

(defn solve2
  ([modules] (reduce + (mapcat fuel-mass-all modules)))
  ([] (solve2 input))) 

