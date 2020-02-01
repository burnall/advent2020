(ns adv.t05
  (:require [adv.util :refer [split-lines split parse-int]]))

(def input
  (->> "data/t05.txt"
       (slurp)
       (#(split % #",|\n"))
       (map parse-int)
       (vec)))
     
(defn run-binary-op [v pos oper modes]
  (let [a (if (zero? (mod modes 10)) 
            (v (v (+ pos 1)))
            (v (+ pos 1)))
        b (if (< modes 10)
            (v (v (+ pos 2)))
            (v (+ pos 2)))]
    (assoc v 
           (v (+ pos 3))
           (oper a b))))

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
                   (if (zero? modes) 
                     (v (v (inc pos)))
                     (v (inc pos))))
      (str "!!!! " op " " v))))

(defn solve 
  ([] (solve input 1))
  ([data param] (next-iter data 0 param)))

