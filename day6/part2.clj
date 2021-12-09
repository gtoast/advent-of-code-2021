(ns day6.part2
  (:require [clojure.string :refer [split-lines split join trim]]))

;gratitude to borkdude's solution

(def test-data [3,4,3,1,2])
(def data (mapv (comp (fn [s] (Integer/parseInt s)) trim) (-> (slurp "day6/input.txt")
                                                              (split #","))))


(defn init [data] (let [fmap (frequencies data)
                        zeroes (into [] (repeat 9 0))]
                    (reduce (fn [acc [k v]]
                              (assoc acc k v)) zeroes fmap)))

(defn lifecycle [data]
  (let [zeroes (nth data 0)]
    (-> (vec (rest data))
        (conj zeroes)
        (update 6 + zeroes))))

(defn lifecycle-for-days [data days]
  (let [data (init data)]
    (nth (iterate lifecycle data) days)))

(defn laternfish-after-days [data days]
  (apply + (lifecycle-for-days data days)))

(def solution (laternfish-after-days data 256))

(comment
  ;init test data
  (init test-data)

  ;iterate 2 days of the life cycle
  (nth (iterate lifecycle (init test-data)) 2) ;; => [1 2 1 0 0 0 1 0 1]
  )