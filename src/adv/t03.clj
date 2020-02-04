(ns adv.t03
  (:require [adv.util :refer [zip split-lines split parse-int]]))

(defn parse-instruction [s]
  (let [dir (first s)
        len (parse-int (subs s 1))]
    {:dx (case dir 
            \R len
            \L (- len)
            0)
     :dy (case dir
           \U len
           \D (- len)
           0)}))

(defn points-to-sections [ps]
  (map (fn [a b] [a b])
       ps
       (rest ps)))

(defn parse-route [s]
  (->> (split s #",")
       (map parse-instruction)
       (reductions (fn [[x y] {:keys [dx dy]}]
                     [(+ x dx) (+ y dy)]) 
               [0 0])
       (points-to-sections)))

(def input
  (->> "data/t03.txt"
       (slurp)
       (split-lines)
       (map parse-route)))

(defn between? [v a b] 
  (or (<= a v b) (<= b v a)))

(defn section-crossing [[[ax ay] [bx by]] [[cx cy] [dx dy]]]
  (cond 
    (and (between? cx ax bx) (between? ay cy dy)) [cx ay] 
    (and (between? ax cx dx) (between? cy ay by)) [ax cy]))

(defn route-crossings [route-a route-b]
  (for [section-a route-a
        section-b route-b
        :let [crossing (section-crossing section-a section-b)]
        :when crossing]
    crossing))    
 
(defn manhattan 
  ([p] (manhattan p [0 0])) 
  ([[x y] [x1 y1]] 
    (+ (Math/abs (- x x1)) (Math/abs (- y y1)))))

(defn solve 
  ([] (apply solve input))
  ([route-a route-b]
    (->> (route-crossings route-a route-b)
         (drop 1)
         (sort-by manhattan))))

; Part 2

(defn section-crossing-with-distance [[[ax ay] [bx by]] [[cx cy] [dx dy]] distance]
  (cond 
    (and (between? cx ax bx) (between? ay cy dy)) 
      (+ distance 
         (Math/abs (- ax cx))
         (Math/abs (- ay cy)))
    (and (between? ax cx dx) (between? cy ay by)) 
      (+ distance
         (Math/abs (- ax cx))
         (Math/abs (- ay cy)))))

(defn distances [route]
  (->> route
       (reductions (fn [distance [a b]]
                     (+ distance (manhattan a b)))
                   0)))

(defn route-crossings-with-distance [route-a route-b]
  (let [distances-a (distances route-a)
        distances-b (distances route-b)]
   
    (for [[distance-a section-a] (zip distances-a route-a)
          [distance-b section-b] (zip distances-b route-b)
          :let [crossing (section-crossing-with-distance section-a section-b (+ distance-a distance-b))]
          :when crossing]
      crossing)))    
 
(defn solve2 
  ([] (apply solve2 input))
  ([route-a route-b]
    (->> (route-crossings-with-distance route-a route-b)
         (drop 1)
         (sort))))

