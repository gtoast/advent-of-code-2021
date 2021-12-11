(ns day7.part1
  (:require [clojure.string :refer [split-lines split join trim]]))

(def test-data "16,1,2,0,4,2,7,1,2,14")
(def data (slurp "day7/input.txt"))

(defn calc-gas-to [n data]
  (reduce (fn [acc d]
            (+ acc (Math/abs (- d n)))) 0 data))


(defn init-data [data]
  (mapv #(Integer/parseInt %)
        (map trim (split data #","))))

(defn total-value-freq-map [freq-map]
  (reduce (fn [acc [k v]] (+ acc (* k v))) 0 freq-map))

(defn sum-torial2 [n]
  (/ (+ (Math/pow n 2) n) 2))

(defn sum-torial
  ([n]
   (sum-torial n 0))

  ([n t]
   (if (= n 0)
     t
     (recur (dec n) (+ t n)))))


(defn sum-frequencies [freq-map]
  (apply + (vals freq-map)))

(defn bigger-than-less-than [curr-val f-map]
  (group-by (fn [[k _]] (> curr-val k)) f-map))

(defn solution [data]
  (let [data (init-data data)
        freq-map (frequencies data)
        sum (sum-freq-map freq-map)]
    ;; freq-map
    ;(- data-sum (* 2 21))
    ;(sort data)
    (bigger-than-less-than 1 freq-map)))



(solution test-data)
(solution data)

(comment

  ;how many ships are there
  (count test-data)

  ;how many unique values are there?
  (count (frequencies test-data))
  ;; => 667

  ;what is the most common value?
  (apply max-key val (frequencies test-data))
  ;; => [1 8]

  ;what is the largest value?
  (apply max-key first (frequencies data))
  ;; => [1876 1]

  ;what is the sum of all the values?
  (reduce (fn [acc [k v]] (+ acc (* k v))) 0 (frequencies data))
  ;; => 482596

  ;what is the median of all the values?
  (double (/ (reduce (fn [acc [k _]] (+ acc k)) 0 (frequencies data))
             (count (frequencies data))))
  ;; => 396689/667
  ;; => 594.736131934033

  (let [data (init-data test-data)
        freq-map (frequencies data)
        bigger-smaller-than-1 (bigger-than-less-than 1 freq-map)]
    (sort-by second (frequencies data)))

  (let [data (init-data data)
        freq-map (frequencies data)
        bigger-smaller-than-1 (bigger-than-less-than 1 freq-map)]

    (apply max data)
    (sort-by second (map (juxt identity #(calc-gas-to % data)) (range (apply max data))))))