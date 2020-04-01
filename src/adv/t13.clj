(ns adv.t13
  (:require [adv.util :refer [split parse-long]])
  (:require [adv.graph :refer [show]]))

(declare execute)

(def input
  (->> "data/t13.txt"
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

(defn execute [storage pos rel-base get-input]
  (let [op (mod (storage pos) 100)
        modes (quot (storage pos) 100)]
    ;(println "[execute] pos:" pos "op:" op "modes" modes "rel-base" rel-base)
    (condp = op
      99 {:halt true}
      1 (recur (run-binary-op storage pos + modes rel-base) 
               (+ pos 4) 
               rel-base
               get-input)       
      2 (recur (run-binary-op storage pos * modes rel-base) 
               (+ pos 4) 
               rel-base
               get-input)
      3 (recur (store storage (inc pos) modes (get-input) rel-base) 
               (+ pos 2) 
               rel-base
               get-input)
      4 {:storage storage
         :pos (+ pos 2)
         :out (get-param storage (inc pos) modes rel-base)
         :rel-base rel-base} 
      5 (recur storage 
               (jump-if-next-pos true storage pos modes rel-base)
               rel-base
               get-input)
      6 (recur storage
               (jump-if-next-pos false storage pos modes rel-base)
               rel-base
               get-input)
      7 (recur (cond-store-data < storage pos modes rel-base)
               (+ pos 4)
               rel-base
               get-input)
      8 (recur (cond-store-data = storage pos modes rel-base)
               (+ pos 4)
               rel-base
               get-input)
      9 (recur storage
               (+ pos 2)
               (+ rel-base (get-param storage (inc pos) modes rel-base))
               get-input)
      (throw (Exception. (str "unknown " op " "))))))

(defn tile-id-color [id]
  (get {0 :light-grey ; an empty tile. No game object appears in this tile. 
        1 :black      ; a wall tile. Walls are indestructible barriers.
        2 :blue       ; a block tile. Blocks can be broken by the ball.
        3 :green      ; a horizontal paddle tile. The paddle is indestructible.
        4 :red        ; a ball tile. The ball moves diagonally and bounces off objects.
        } id :yellow))

(defn tiles-to-wall [tiles]
  (->> tiles
       (map (fn [[x y tile-id]] 
              [[x y] tile-id]))
       (into {})))
       
(defn rect-within [wall]
  (let [ps (keys wall)]
    {:xmin (dec (first (apply min-key first ps)))
     :xmax (inc (first (apply max-key first ps)))
     :ymin (dec (second (apply min-key second ps)))
     :ymax (inc (second (apply max-key second ps)))}))

(defn collect-outputs [data]
  (->> {:storage (storage data)
        :pos 0
        :out 0
        :rel-base 0
        :input nil}
       (iterate (fn [{:keys [storage pos rel-base]}]
                  (execute storage pos rel-base nil)))
       (drop 1)
       (take-while (complement :halt))
       (map :out)))
                  
(defn get-wall [data]
  (->> data
       (collect-outputs)
       (partition 3)
       (tiles-to-wall)))

(defn draw [data]
  (let [wall (get-wall data)
        bounds (rect-within wall)]
    (show wall bounds tile-id-color)))

(defn solve
  ([] (solve input)) 
  ([data] 
    (->> data
         (get-wall) 
         (filter (fn [[_ tile-id]] (= tile-id 2)))
         (count))))

(defn parse-output [output]
  (let [triples (partition 3 output)
        [paddle-x _ _] (last (filter #(= 3 (last %)) triples))
        [ball-x _ _] (last (filter #(= 4 (last %)) triples))
        scores (mapcat (fn [[x y i]] (when (and (= x -1) (= y 0)) [i])) triples)
        has-blocks? (some #(= 2 (last %)) triples)]
    {:paddle-x paddle-x 
     :ball-x ball-x
     :scores scores
     :has-blocks? has-blocks?}))

(defn play-round [storage input all-scores rel-base]
  ;(prn 22 input all-scores)
  (let [{new-storage :storage, out :outputs, new-rel-base :rel-base} (execute storage 0 [] rel-base input)
        {:keys [ball-x paddle-x scores has-blocks?]} (parse-output out)]
    out))
    ;(println ball-x paddle-x scores has-blocks? new-rel-base)
    ;(if has-blocks?
    ;  (recur new-storage 
    ;         (Integer/compare ball-x paddle-x)
    ;         (concat all-scores scores)
    ;         rel-base)
    ;  (concat all-scores scores)))) 
      
(defn solve2
  ([] (solve2 (assoc input 0 2)))
  ([data] 
    (play-round (storage data) 0 [] 0)))

