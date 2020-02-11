(ns adv.t08 (:require [clojure.string :refer [join trim-newline]]))

(def input 
  (->> "data/t08.txt"
       (slurp)
       (trim-newline)))

(defn solve
  ([] (solve input (* 25 6)))
  ([data size] 
    (->> data
         (partition size)
         (map frequencies)
         (apply min-key #(get % \0))
         ((fn [m] (* (m \1) (m \2)))))))

(defn render [layers]
  (apply map
     (fn [& ps] 
       (first (filter (partial not= \2) ps)))
     layers))

(defn draw-line [ps]
  (->> ps
       (map #(if (= % \1) \x \space))
       (join)))

(defn draw [width data]
  (->> data
       (partition width)
       (map draw-line)
       (join "\n")
       (println)))

(defn solve2
  ([] (solve2 input 25 6))
  ([data width height]
    (->> data
         (partition (* width height))
         (render)
         ((partial draw width)))))

