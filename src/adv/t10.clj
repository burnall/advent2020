(ns adv.t10 (:require [adv.util :refer [split-lines]]))

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

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)
       (to-data)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b)))) 

(defn ast-visible? [terrain [xa ya] [xb yb]]
  (if (and (= xa xb) (= ya yb))
    false 
    (let [dx (- xb xa)
          dy (- yb ya)
          n (gcd (Math/abs dx) (Math/abs dy))]
      (->> (range 1 n)
           (some (fn [i] (get-in terrain [(+ ya (* (/ dy n) i)) 
                                           (+ xa (* (/ dx n) i))])))
           (not)))))                               

(defn best-location [{:as data, :keys [terrain asteroids]}]
  (->> asteroids
       (map (fn [ast-a] 
              (->> asteroids
                   (filter (partial ast-visible? terrain ast-a))
                   (count)
                   ((fn [cnt] {:p ast-a, :cnt cnt}))
                   )))
       (sort-by (comp - :cnt))
       (first)
  ))

(defn solve
  ([] (solve input))
  ([data] 
    (->> data
         (best-location))))

(defn manhattan [[xa ya] [xb yb]]
  (+ (Math/abs (- xa xb)) (Math/abs (- ya yb)))) 

(defn quarter [[x0 y0] [x y]]
  (if (>= x x0)
    (if (> y y0)
      2
      1)
    (if (> y y0)
      3
      4)))

(defn signum [x]
  (cond 
    (zero? x) 0
    (> x 0) 1 
    :else -1))

(defn angle [[x0 y0] [x y]]
  (if (= y y0)
    (* 1000 (signum (- x x0)))
    (/ (- x0 x) (- y y0))))    

(defn to-polar [p0 p]
  [(quarter p0 p) (angle p0 p)])

(defn find-next-not-empty-col-index [v start-idx]
  (let [next-idx (mod (inc start-idx) (count v))]
    (if (seq (v next-idx))
      next-idx
      (recur v next-idx))))

(defn next-victim [[idx ast-by-angles]]
  [(find-next-not-empty-col-index ast-by-angles idx)
   (assoc ast-by-angles idx (rest (ast-by-angles idx)))])

(defn solve2 
  ([] (solve2 input 199))
  ([data idx]
    (let [ast0 (:p (best-location data))
          ast-by-angles 
            (->> data
                 (:asteroids)
                 (filter (partial not= ast0))
                 (group-by (partial to-polar ast0))
                 (sort)
                 (map second)
                 (mapv (partial sort-by (partial manhattan ast0))))]
      (->> [0 ast-by-angles]
          (iterate next-victim)
           (#(nth % idx))
           ((fn [[current aba]] (first (aba current))))))))

