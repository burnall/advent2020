(ns adv.t02
  (:require [adv.util :refer [split parse-int]]))

(defn fix [v a b] 
  (assoc (assoc v 1 a) 
         2 
         b))

(def input
  (->> "data/t02.txt"
       (slurp)
       (#(split % #",|\n"))
       (map parse-int)
       (vec)))
     
(defn run-op [v pos oper] 
  (assoc v 
         (v (+ pos 3))
         (oper (v (v (+ pos 1))) 
               (v (v (+ pos 2))))))

(defn iter [v pos]
  (try
    (condp = (v pos)
      99 (v 0) 
      1 (iter (run-op v pos +) (+ pos 4))       
      2 (iter (run-op v pos *) (+ pos 4))
     "!!!!")
   (catch Exception e "????")))

(defn solve 
  ([] (solve (fix input 12 2)))
  ([data] (iter data 0)))

(def pairs 
  (->> 1000
       (range)
       (mapcat (fn [m] (map (fn [k] [k (- m k)]) 
                            (range (inc m)))))))

(defn filter-by-target [data target [a b]]
  (when (= (iter (fix data a b) 0) 
           target) 
    (+ (* 100 a) b)))

(defn solve2 
  ([] (solve2 input 19690720))
  ([data target] 
    (->> pairs
         (filter (partial filter-by-target input target)) 
         (first))))

