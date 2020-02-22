(ns adv.t12
  (:require [adv.util :refer [split-lines parse-int]]))

(defn make-moons [positions]
  (->> positions
       (map (partial assoc {:vel [0 0 0]} :pos)))) 

(def input
  (->> "data/t12.txt"
       (slurp)
       (split-lines)
       (map (partial re-seq #"<x=(\S*), y=(\S+), z=(\S+)>"))
       (map (comp (partial map parse-int) rest first))
       (make-moons)))

(defn apply-gravity [moons {:keys [pos vel]}]
  (->> moons
       (map :pos)
       (map (partial map (fn [a b]
                           (cond 
                             (= a b) 0
                             (< a b) 1
                             :else -1))
                         pos))
       (apply map + vel)))  
                        

(defn do-step [moons]
  (let [vels (map (partial apply-gravity moons) moons)]
    ; seq fix StackOverflow due to mix of lazy and eager
    (seq (map (fn [vel {pos :pos}]
           {:pos (map + pos vel), :vel vel})  
         vels
         moons))))

(defn energy [v]
  (reduce (fn [agg el]
            (+ agg (Math/abs el)))
          0
          v))  
     
(defn total-energy [moons]
  (->> moons
       (map (fn [{:keys [pos vel]}]
              (* (energy pos) (energy vel))))
       (reduce +)))

(defn solve
  ([] (solve input 1000))
  ([moons steps] 
    (->> moons
         ;(iter do-step steps)
         (iterate do-step)
         ;(take (inc steps))
         ;(last)
         (#(nth % steps)) 
         (total-energy))))

