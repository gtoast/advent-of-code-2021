(ns day6.part1
  (:require [clojure.string :refer [split-lines split join trim]]))

(def test-data [3,4,3,1,2])

(def input (mapv #(Integer/parseInt %) (map trim (split (slurp "day6/input.txt") #","))))

(defn lifecycle [n]
  (cond
    (< (dec n) 0) [6, 8]
    :else (dec n)))


(defn cycle-all [fishes]
  (let [next-fishes (map lifecycle fishes) ;cycling fish come out in [6 8] pairs
        fishes-and-new-fishes (reduce 
                           (fn [acc f]
                             (if (vector? f)
                                (-> acc
                                     (update 0 conj (first f))
                                     (update 1 conj (second f)))
                                 (update acc 0 conj f))) 
                               [[] []] ;our accumulator a list for fishes 
                                       ; and a list for new fish to be added 
                               next-fishes)]
    (apply concat fishes-and-new-fishes)))

(defn cycle-all-for-days [days inp]
  (take (inc days) (iterate cycle-all inp)))

(defn solution [days inp]
  (count (last (cycle-all-for-days days inp))))

(defn -main [args]
  (solution 80 test-data))

(comment

  ;apply a day's cycle to all fishes
  (cycle-all '(2 3 2 0 1))

  ;show results of cycling for number of days
  (cycle-all-for-days 18 '(3 4 3 1 2))

  ;show results of last day
  (last (cycle-all-for-days 80 '(3 4 3 1 2)))

  (-main [])
  )