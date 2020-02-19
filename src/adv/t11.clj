(ns adv.t11
  (:require [adv.util :refer [split parse-long]])
  (:require [adv.graph :refer [show]]))

(declare execute)

(def input
  (->> "data/t11.txt"
       (slurp)
       (#(split % #",|\n"))
       (map parse-long)
       (vec)))

(defn throw-neg-exception [i]
  (throw (Exception. (str "Access by negative index " i))))

(defn storage 
  ([v] (storage v {}))
  ([v mem] (fn
    ([] mem)
    ([i]
      (cond
        (< i 0) (throw-neg-exception i)  
        (< i (count v)) (v i) 
        :else (get mem i 0)))
    ([i value]
      ;(println "writing to" i "value" value)
      (cond
        (< i 0) (throw-neg-exception i) 
        (< i (count v)) (storage (assoc v i value) mem)
        :else (storage v (assoc mem i value)))))))

(defn get-param [storage pos mode rel-base]
  (let [el (storage pos)]
    (condp = mode 
      0 (storage el)
      1 el
      2 (storage (+ rel-base el)))))

(defn get-two-params [storage pos modes rel-base]
  [(get-param storage (+ pos 1) (mod modes 10) rel-base)
   (get-param storage (+ pos 2) (mod (quot modes 10) 10) rel-base)]) 

(defn store [storage pos mode value rel-base] 
  (let [idx (condp = mode
              0 (storage pos)
              2 (+ rel-base (storage pos))
              (throw (Exception. (str "Incorrect mod for store call " mode))))]
    (storage idx value)))            
    
(defn run-binary-op [storage pos oper modes rel-base]
  (let [[a b] (get-two-params storage pos modes rel-base)]
    (store storage (+ pos 3) (quot modes 100) (oper a b) rel-base)))

(defn jump-if-next-pos [value storage pos modes rel-base] 
  (let [a (get-param storage (+ pos 1) (mod modes 10) rel-base)]
    (if (= value (not= a 0))
      (get-param storage (+ pos 2) (quot modes 10) rel-base)
      (+ pos 3))))        
        
(defn cond-store-data [pred storage pos modes rel-base]
  (let [[a b] (get-two-params storage pos modes rel-base)]
    (store storage 
           (+ pos 3) 
           (quot modes 100) 
           (if (pred a b) 1 0)
           rel-base)))

(defn new-pos [[x y] grad]
  (condp = grad
    0 [x (inc y) ]
    1 [(inc x) y]
    2 [x (dec y)]
    3 [(dec x) y]))

(defn robot [wall pos grad color] 
  (fn 
    ([] (get wall pos 0))
    ([out] 
      (if color
        (let [grad' (mod (+ grad out out -1) 4)]
          (robot (assoc wall pos color) 
                 (new-pos pos grad')
                 grad' 
                 nil))
        (robot wall pos grad out)))
    ([a b] wall)))

(defn execute [storage pos robot rel-base]
  (let [op (mod (storage pos) 100)
        modes (quot (storage pos) 100)]
    ;(println "[execute] pos:" pos "op:" op "modes" modes "rel-base" rel-base)
    (condp = op
      99 (robot 0 0)
      1 (recur (run-binary-op storage pos + modes rel-base) 
               (+ pos 4) 
               robot 
               rel-base)       
      2 (recur (run-binary-op storage pos * modes rel-base) 
               (+ pos 4) 
               robot 
               rel-base)
      3 (recur (store storage (inc pos) modes (robot) rel-base) 
                 (+ pos 2) 
                 robot
                 rel-base)
      4 (recur storage 
               (+ pos 2)
               (robot (get-param storage (inc pos) modes rel-base))
               rel-base) 
      5 (recur storage 
               (jump-if-next-pos true storage pos modes rel-base)
               robot 
               rel-base)
      6 (recur storage
               (jump-if-next-pos false storage pos modes rel-base)
               robot
               rel-base)
      7 (recur (cond-store-data < storage pos modes rel-base)
               (+ pos 4)
               robot
               rel-base)
      8 (recur (cond-store-data = storage pos modes rel-base)
               (+ pos 4)
               robot
               rel-base)
      9 (recur storage
               (+ pos 2)
               robot
               (+ rel-base (get-param storage (inc pos) modes rel-base)))
      (throw (Exception. (str "unknown " op " "))))))

;(def input [104,1125899906842624,99])
;(def input [109 19 204 -15 99])
;(def input [109 25 203 -25 70 4 0 99]) 
;(def input [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

(defn solve
  ([] (solve input)) 
  ([data] 
    (execute (storage data) 
             0 
             (robot {} [0 0] 0 nil) 
             0)))

(defn rect-within [wall]
  (let [ps (keys wall)]
    {:xmin (dec (first (apply min-key first ps)))
     :xmax (inc (first (apply max-key first ps)))
     :ymin (dec (second (apply min-key second ps)))
     :ymax (inc (second (apply max-key second ps)))}))

(defn solve2
  ([] (solve2 input))
  ([data]
    (let [wall (execute (storage data) 
                        0 
                        (robot {[0 0] 1} [0 0] 0 nil) 
                        0)
          bounds (rect-within wall)]
      (show wall bounds))))    

