(ns adv.t02
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)
       (map parse-int)))


