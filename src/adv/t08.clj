(ns adv.t08)

(def input 
  (->> "data/t08.txt"
       (slurp)
       (clojure.string/trim-newline)))

(defn solve
  ([] (solve input 25 6))
  ([data width height] 
    (->> data
         (partition (* width height))
         (map frequencies)
         (apply min-key #(get % \0))
         ((fn [m] (* (m \1) (m \2))))
         )))

