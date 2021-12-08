(ns day1
  (:require [clojure.string :refer [split-lines]]))

(def raw-in (-> (slurp "input.txt") split-lines))

(def input (map #(java.lang.Integer/parseInt %) raw-in))

(def pairs (partition 2 1 input))

(def diffs (map #(apply - %) pairs))


(defn negs [s](map neg? s))

(def counts (frequencies negs))


(def tuples (partition 3 1 input))

(def tuple-pairs (partition 2 1 tuples))

(def diff-of-tuple (map (fn [[fst snd]]
                          (- (apply + fst)
                             (apply + snd))) tuple-pairs))


;; (comment
;;   (map neg?)

;;   frequencies
;;   )
