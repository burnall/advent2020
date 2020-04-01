(ns adv.joda
  (:require [clojure.string :refer [split upper-case lower-case join]]))

(defn modify-1st [f s]
  (str (f (subs s 0 1)) (subs s 1)))

(defn ayoda-sentence [sentence]
  (let [[a b] (split sentence #", ")]
    (if b
      (str (modify-1st upper-case b) " " (modify-1st lower-case a))
      a)))
        
(defn ayoda [text]
  (->> (split (str text " ") #"\. ")
       (map ayoda-sentence)
       (join ". ")
       ((fn [s] (str s ".")))))

