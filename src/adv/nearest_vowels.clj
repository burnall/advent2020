(ns adv.nearest-vowels)

(defn f [s]
  (let [vowels #{\a \e \i \o \u}
        vowels-idx (mapcat (fn [i ch] (when (vowels ch) [i]))
                           (range)
                           s)
        handle-range (fn [from to]
                        (cond 
                          (nil? from) (range to 0 -1)
                          (nil? to) (range 0 (- (count s) from))
                          :else (map #(Math/min (- % from) (- to %))
                                     (range from to))))
       ]
    (mapcat handle-range
            (cons nil vowels-idx)
            (concat vowels-idx [nil]))))
