(ns day3.part1
  (:require [clojure.string :refer [split-lines split join]]))

(defn massaged-input [s]
  (->> s
       split-lines))

(def raw-data (slurp "day3/input.txt"))

(def raw-test-data
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def input (massaged-input raw-data))

(def vert-input (apply map vector input))

(def gamma (->> vert-input
                (map frequencies)
                (map #(apply max-key val %))
                (map key)
                join))

(def epsilon (->> vert-input
                  (map frequencies)
                  (map #(apply min-key val %))
                  (map key)
                  join))

(def solution (* (java.lang.Long/parseLong gamma 2)
                 (java.lang.Long/parseLong epsilon 2)))