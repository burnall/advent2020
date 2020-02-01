(ns adv.t04)

(defn digits [n]
  (->> [n []]
       (iterate (fn [[n digits]] [(quot n 10) (conj digits (mod n 10))]))
       (some (fn [[n digits]] (when (zero? n) digits)))))
       
(defn two-adjacent? [n]
  (->> (digits n)
       (reduce (fn [prev d] 
                 (if (= prev d) 
                   (reduced true)
                   d)))
       (true?)))

(defn increasing? [n]
  (apply >= (digits n)))

(defn solve [from to]
  (->> (range from to)
       (filter #(and (two-adjacent? %) (increasing? %)))))

(defn exactly-two-adjacent? [n] 
  (->> (digits n)
       (partition-by identity)
       (some (fn [coll] (= (count coll) 2)))))

(defn solve2 [from to]
  (->> (range from to)
       (filter #(and (exactly-two-adjacent? %) (increasing? %)))))
 
