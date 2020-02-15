(ns adv.t10 (:require [adv.util :refer [split-lines]]))

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)))

(defn asteroids [terrain]
  (for [x (range (count (terrain 0))) 
        y (range (count terrain))
        :when (get-in terrain [y x])]
    [x y]))      

(defn to-data [raw]
  (->> raw
       (mapv (partial mapv (partial = \#)))
       ((fn [terrain] {:terrain terrain, :asteroids (asteroids terrain)}))
       ))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b)))) 

(defn -get-in [terrain p]  
  (get-in terrain p))

(defn ast-visible? [terrain [xa ya] [xb yb]]
  (if (and (= xa xb) (= ya yb))
    false 
    (let [dx (- xb xa)
          dy (- yb ya)
          n (gcd (Math/abs dx) (Math/abs dy))]
      (->> (range 1 n)
           (some (fn [i] (-get-in terrain [(+ ya (* (/ dy n) i)) 
                                           (+ xa (* (/ dx n) i))])))
           (not)))))                               

(defn best-location [{:as data, :keys [terrain asteroids]}]
  (->> asteroids
       (map (fn [ast-a] 
              (->> asteroids
                   (filter (partial ast-visible? terrain ast-a))
                   (count))))
       (sort-by -)            
  ))

(defn solve
  ([] (solve input))
  ([raw] 
    (->> raw
         (to-data)
         (best-location))))

