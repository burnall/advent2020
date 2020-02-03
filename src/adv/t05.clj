(ns adv.t05
  (:require [adv.util :refer [split parse-int]]))

(def input
  (->> "data/t05.txt"
       (slurp)
       (#(split % #",|\n"))
       (map parse-int)
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

(defn jump-if [value v pos modes] 
  (let [a (get-param v (+ pos 1) (mod modes 10))
        next-pos 
          (if (= value (not= a 0))
            (get-param v (+ pos 2) (quot modes 10))
            (+ pos 3))]
    (next-iter v next-pos nil)))        
        
(defn cond-store [pred v pos modes]
  (let [[a b] (get-two-params v pos modes)]
    (next-iter (assoc v 
                      (v (+ pos 3)) 
                      (if (pred a b) 1 0))
               (+ pos 4)
               nil)))

(defn next-iter [v pos param]
  (let [op (mod (v pos) 100)
        modes (quot (v pos) 100) cc (prn pos op)]
    (condp = op
      99 param 
      1 (next-iter (run-binary-op v pos + modes) (+ pos 4) nil)       
      2 (next-iter (run-binary-op v pos * modes) (+ pos 4) nil)
      3 (if param
          (next-iter (assoc v (v (inc pos)) param) (+ pos 2) nil)
          "no input")
      4 (next-iter v 
                   (+ pos 2) 
                   (get-param v (inc pos) modes)) 
      5 (jump-if true v pos modes) 
      6 (jump-if false v pos modes)
      7 (cond-store < v pos modes) 
      8 (cond-store = v pos modes)
      (str "!!!! " op " " v))))

(defn solve 
  ([] (solve input 1))
  ([data param] (next-iter data 0 param)))

