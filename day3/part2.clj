(ns day3.part2
  (:require [clojure.string :refer [split-lines split join]]))

(def debug false)

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

(defn vert-input [inp]
  (apply map vector inp))

(defn equal-freq? [m] (apply = (vals m)))
(defn most-freq-bit [m] (key (apply max-key val m)))
(defn least-freq-bit [m] (key (apply min-key val m)))

(defn most-common-bit-or-1 [inp]
  (let [freq (frequencies inp)]
    (if (equal-freq? freq)
      \1
      (most-freq-bit freq))))

(defn least-common-bit-or-0 [inp]
  (let [freq (frequencies inp)]
    (if (equal-freq? freq)
      \0
      (least-freq-bit freq))))

(defn mult-binary [m n]
  (* (java.lang.Long/parseLong m 2)
     (java.lang.Long/parseLong n 2)))

;; (def gamma (->> vert-input
;;                 (map frequencies)
;;                 (map #(apply max-key val %))
;;                 (map key)
;;                 join))

;; (def epsilon (->> vert-input
;;                   (map frequencies)
;;                   (map #(apply min-key val %))
;;                   (map key)
;;                   join))


(defmulti bit-criteria (fn [type _ _] type))

(defmethod bit-criteria :oxy-gen [type inp curr-pos]
  (let [v-inp (vert-input inp)]
    (most-common-bit-or-1 (nth v-inp curr-pos))))

(defmethod bit-criteria :co2-scrub [type inp curr-pos]
  (let [v-inp (vert-input inp)]
    (least-common-bit-or-0 (nth v-inp curr-pos))))

(defn get-oxy-gen-rating [input]
  (let [diagnostic-numbers (atom input)]
    (doseq [curr-pos (range (count (nth @diagnostic-numbers 0)))
            :when (> (count @diagnostic-numbers) 1)
            :let [bit-crit (bit-criteria :oxy-gen @diagnostic-numbers curr-pos)]]
      (when debug (prn  "in: " (count @diagnostic-numbers) "curr-pos: " curr-pos "bc: " bit-crit "diagnums: " @diagnostic-numbers))
      (swap! diagnostic-numbers (fn [n] (filter #(= (.charAt % curr-pos) bit-crit) n))))
    @diagnostic-numbers))

(defn get-co2-scrub-rating [input]
  (let [diagnostic-numbers (atom input)]
    (doseq [curr-pos (range (count (nth @diagnostic-numbers 0)))
            :when (> (count @diagnostic-numbers) 1)
            :let [bit-crit (bit-criteria :co2-scrub @diagnostic-numbers curr-pos)]]
      (when debug (prn  "in: " (count @diagnostic-numbers) "curr-pos: " curr-pos "bc: " bit-crit "diagnums: " @diagnostic-numbers))
      (swap! diagnostic-numbers (fn [n] (filter #(= (.charAt % curr-pos) bit-crit) n))))
    @diagnostic-numbers))

(def co2-scrub (first (get-co2-scrub-rating input)))
(def oxy-gen (first (get-oxy-gen-rating input)))

(def solution (mult-binary co2-scrub oxy-gen))

(comment
  (bit-criteria :oxy-gen input 0)
  (bit-criteria :co2-scrub input 0))