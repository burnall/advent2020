(ns adv.t07
  (:require [adv.util :refer [split parse-int permutations]]))

(declare execute)

(def input
  (->> "data/t07.txt"
       (slurp)
       (#(split % #",|\n"))
       (map parse-int)
       (map long)
       (vec)))

(defn get-param [v pos mode]
  (let [el (v pos)]
    (if (zero? mode) 
      (v el)
      el)))

(defn get-two-params [v pos modes]
  [(get-param v (+ pos 1) (mod modes 10))
   (get-param v (+ pos 2) (quot modes 10))]) 
     
(defn run-binary-op [v pos oper modes]
  (let [[a b] (get-two-params v pos modes)]
    (assoc v 
           (v (+ pos 3))
           (oper a b))))

(defn jump-if-next-pos [value v pos modes] 
  (let [a (get-param v (+ pos 1) (mod modes 10))]
    (if (= value (not= a 0))
      (get-param v (+ pos 2) (quot modes 10))
      (+ pos 3))))        
        
(defn cond-store-data [pred v pos modes]
  (let [[a b] (get-two-params v pos modes)]
    (assoc v 
           (v (+ pos 3)) 
           (if (pred a b) 1 0))))

(def pause-on-output true)
(defn execute [v pos in-params out-param]
  (let [op (mod (v pos) 100)
        modes (quot (v pos) 100)]
    ;(println "execute" pos op in-params out-param)
    (condp = op
      99 {:out out-param, :data v}
      1 (recur (run-binary-op v pos + modes) (+ pos 4) in-params nil)       
      2 (recur (run-binary-op v pos * modes) (+ pos 4) in-params nil)
      3 (if (seq in-params)
          (recur (assoc v (v (inc pos)) (first in-params)) 
                 (+ pos 2) 
                 (rest in-params)
                 nil)
          (throw (Exception. "no input")))
      4 (if pause-on-output
          {:out (get-param v (inc pos) modes)
           :data v
           :pos (+ pos 2)
           :suspended true}
          (recur v 
                 (+ pos 2)
                 in-params
                 (get-param v (inc pos) modes))) 
      5 (recur v 
               (jump-if-next-pos true v pos modes)
               in-params
               nil)
      6 (recur v
               (jump-if-next-pos false v pos modes)
               in-params
               nil)
      7 (recur (cond-store-data < v pos modes)
               (+ pos 4)
               in-params
               nil)
      8 (recur (cond-store-data = v pos modes)
               (+ pos 4)
               in-params
               nil)
      (throw (Exception. (str "!!!! " op " " v))))))

;(def input [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
;(def input [3,23,3,24,1002,24,10,24,1002,23,-1,23,
;101,5,23,23,1,24,23,23,4,23,99,0,0])
;(def input [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
;  27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
;(def input [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
;-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
;53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

(defn run [data setting2 setting1]
  (:out (execute data 0 (list setting1 setting2) nil)))

(defn solve 
  ([] (solve input))
  ([data] 
    (->> [0 1 2 3 4]
         (permutations)
         ;[[4 3 2 1 0]]
         (map (partial reduce (partial run data) 0))
         (apply max)
         )))

(defn run-loop 
  ([data phase-settings] 
    (run-loop (vec (repeat 5 {:data data, :pointer 0})) 0 (vec phase-settings) 0 nil))
  ([datas inp phase-settings idx curr-signal] ;(println "run-loop" idx) 
    (let [{:keys [out data pos suspended]} 
      (execute (:data (datas idx))
               (:pointer (datas idx))
               ;(list (phase-settings idx) inp))]
               (if (seq phase-settings)
                 (list (first phase-settings) inp)
                 (list inp))
               nil)]
      (if suspended
        (recur (assoc datas idx {:data data, :pointer pos})
               out
               (rest phase-settings)
               (mod (inc idx) 5)
               (if (= idx 4) out curr-signal))
        curr-signal))))

(defn solve2 
  ([] (solve2 input))
  ([data] 
    (->> [5 6 7 8 9]
         (permutations)
         ;[[9 7 8 5 6]]
         (map (partial run-loop data))
         (apply max)
         )))

