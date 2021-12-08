(ns day2.part1
  (:require [clojure.string :refer [split-lines split]]))

(defn massaged-input [s]
  (->> s
       split-lines
       (map #(split % #" "))
       (map (fn [l] (update l 1 #(Integer/parseInt %))))))

(def raw-data (slurp "input.txt"))

(def raw-test-data "forward 5
down 5
forward 8
up 3
down 8
forward 2")

;; (def input (massaged-input raw-test-data))

(def input (massaged-input raw-data))


(def start {:hor 0 :dep 0})

(defmulti pilot (fn [_ comm] (first comm)))

(defmethod pilot "forward" [pos [_ amount]]
  (update pos :hor #(+ amount %)))

(defmethod pilot "down" [pos [_ amount]]
  (update pos :dep #(+ amount %)))

(defmethod pilot "up" [pos [_ amount]]
  (update pos :dep #(- % amount)))


(pilot start ["forward" 8])

(def final-loc (reduce pilot start input))

(* (:hor final-loc) (:dep final-loc))


