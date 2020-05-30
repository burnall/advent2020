(ns adv.t14
  (:require [adv.util :refer [split split-lines parse-int]]
            [clojure.string :refer [lower-case]]
            [clojure.set :refer [difference]]
            ))

; 9 TLFN => [\TLFN 9]
(defn parse-element [s]
  (let [[a b] (split s #" ")]
    [(keyword (lower-case b)) (parse-int a)]))

(defn parse-clause [s]
  (->> (split s #", ")
       (map parse-element)
       (into {})))

(defn parse-reaction [s]
  (let [[from to] (split s #" => ")
        [to-chemical q] (first (parse-clause to))]
    [to-chemical {:q q
                  :from (parse-clause from)}]))

(def input
  (->> "data/t14.txt"
       (slurp)
       (split-lines)
       (map parse-reaction)
       (into {})))

(def initial-state
  {:elems {:fuel 1}
   :ore 0})

(defn next-ranks [reactions ranks new-rank]
  (->> reactions
       (mapcat (fn [[chemical {from :from}]]
                 (when (and (not (ranks chemical))
                            (empty? (difference (set (keys from)) (set (keys ranks))))) [chemical])))
       (map #(vector % new-rank))
       (into {})))

(defn build-ranks 
  ([reactions ranks new-rank]
    (let [fresh-ranks (next-ranks reactions ranks new-rank)]
      (if (empty? fresh-ranks)
         ranks
         (recur reactions (merge ranks fresh-ranks) (inc new-rank)))))
  ([reactions] (build-ranks reactions {:ore 0} 1)))        

(defn round-up [a b]
  (+ (quot a b)
     (if (zero? (mod a b)) 0 1)))

(defn make [reactions chemical need-q]
  (let [{from :from, produced-q :q} (reactions chemical)
        runs (+ (round-up need-q produced-q))]
    (->> from
         (map (fn [[ch q]] [ch (* q runs)]))
         (into {}))))
        
(defn next-production [reactions chemicals ranks]
  (let [elems (keys chemicals)
        max-rank (ranks (apply max-key ranks elems))
        chemicals-to-make (filter #(= max-rank (ranks %)) 
                                  elems)
        aftermath (map #(make reactions % (chemicals %))
                       chemicals-to-make)
        new-chemicals (apply merge-with + 
                                        (apply dissoc chemicals chemicals-to-make)
                                        aftermath)]
    (if (and (= 1 (count new-chemicals)) (:ore new-chemicals))
      (:ore new-chemicals)
      (recur reactions new-chemicals ranks))))

(defn solve [fuel]
  (next-production input {:fuel fuel} (build-ranks input)))

; f(a)<0 and f(b)>0 find x, so f(x)<=0 and f(x+1)>0. f - monotonously growing 
(defn binary-search [f a b] 
  (if (= (inc a) b)
    a
    (let [mid (quot (+ a b) 2)]
      (if (> (f mid) 0)
        (recur f a mid)
        (recur f mid b)))))
 
(defn solve2 [ore low high]
  (letfn [(f [fuel] (- (solve fuel) ore))]
    (binary-search f low high)))

