(ns adv.t12
  (:require [adv.util :refer [split-lines parse-int gcd]]))

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
         (iterate do-step)
         (#(nth % steps)) 
         (total-energy))))

; Part 2

(defn signum [a b]
  (cond 
    (= a b) 0
    (< a b) 1
    :else -1))

(defn apply-gravity-1-dim [ps p v]
  (->> ps
       (map (partial signum p))
       (reduce + v)))

(defn iter-until [xs pred]
  (->> (map (fn [x i] [x i]) xs (range))
       (drop 1)
       (drop-while (comp not pred first))
       (first)
       (second)))

(defn do-step-1-dim [{:keys [ps vs]}]
  (let [vs' (map (partial apply-gravity-1-dim ps) ps vs)]
    {:ps (map + ps vs'), :vs vs'}))

(defn find-period [state]
  (let [start-pos (:ps state)] 
    (iter-until (iterate do-step-1-dim state) (partial = state))))

(defn split-by-dimensions [moons]
  (map (fn [i]
         {:vs (map #(nth (:vel %) i) moons)
          :ps (map #(nth (:pos %) i) moons)})
       (range 3)))

(defn lcm [v]
  (reduce (fn [agg n]
            (/ (* agg n) (gcd agg n)))
          v))

(defn solve2
  ([] (solve2 input))
  ([moons] 
    (->> moons 
         (split-by-dimensions)
         (map find-period)
         (lcm)
         )))

