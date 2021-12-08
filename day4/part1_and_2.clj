(ns day4.part1-and-2
  (:require [clojure.string :refer [split-lines split join triml]]
            [clojure.pprint :refer [print-table pprint pp]]))

(def test-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")

(def input (slurp "day4/input.txt"))

(defn massage-input [inp]
  (clojure.string/split inp #"\n\n"))

(defn comma-sep-number-string-to-seq [inp]
  (map #(Integer/parseInt %) (split inp #",")))

(defn board-string-to-spaces [raw-board]
  (->> (split raw-board #"\n")
       (map triml)
       (map #(split % #"\s+"))
       (mapv (fn [row] (mapv #(hash-map :marked false :num (Integer/parseInt %)) row)))))

(defn sing-board [raw-board]
  (as-> raw-board board
    (clojure.string/replace board #"\n" " ")
    (triml board)
    (split board #"\s+")
    (mapv #(hash-map :marked false :num (Integer/parseInt %)) board)))

(defn single-all-boards [raw-boards]
  (mapv sing-board raw-boards))

(defn map-input [inp]
  (let [split-input (massage-input inp)]
    (assoc {}
           :draw-numbers (comma-sep-number-string-to-seq (first split-input))
           :boards (single-all-boards (rest split-input)))))

(defn get-board [map-inp n]
  (get (:boards map-inp) n))

(defn as-rows [sing-board]
  (mapv into (repeat []) (partition 5 sing-board)))

(defn as-columns [sing-board]
  (apply mapv vector (partition 5 sing-board)))

(defn get-row-winners [sing-board]
  (let [rows (as-rows sing-board)]
    (for [row rows]
      (every? :marked row))))

(defn get-column-winners [sing-board]
  (let [rows (as-columns sing-board)]
    (for [row rows]
      (every? :marked row))))


(defn mark-space [board num]
  (map (fn [space]
         (if (= num (:num space))
           (assoc space :marked true)
           space)) board))

(defn mark-boards [boards n]
  (map #(mark-space % n) boards))

(defn draw-numbers-on-boards [boards nums]
  (if (empty? nums)
    boards
    (recur (mark-boards boards (first nums)) (rest nums))))

(def mapped-input (map-input input))
(def boards (:boards mapped-input))
(def draw-numbers (:draw-numbers mapped-input))






(comment
  (def split-test (massage-input test-input))

  (def board-1 (get-in mapped-input [:boards 0]))
  (def board-1-sing-row-with-spaces (sing-board board-1))
  (def marked-board-1 (mark-space board-1-sing-row-with-spaces 22))
  (def mark-boards-test (mark-boards (:boards mapped-input) 22))
  (def board-space {:num 22 :marked false})

  (get-in (mapv as-rows boards) [0 0])  ;get me the first row of the first board
  (get-in (mapv as-columns boards) [0 0]) ;get me the first column of the first board
  )



(def board-atom (atom boards))
(def winning-number (last (for
                           [draw-num draw-numbers :while (not-any? true? (concat (mapcat get-column-winners @board-atom) (mapcat get-row-winners @board-atom)))]
                            (do (swap! board-atom mark-boards draw-num)
                                draw-num))))


(defn get-winning-boards [boards] (remove nil? (for [board boards]
                                                 (when (some true? (apply concat ((juxt get-column-winners get-row-winners) board)))
                                                   board))))

;; (defn find-last-board [{:keys [boards draw-numbers]}]
;;   (loop [bs boards dns draw-numbers]
;;     (let [drawn-number (first dns)
;;           marked-boards (mark-boards bs drawn-number)]
;;       (if-some [winning-boards (get-winning-boards marked-boards)]
;;         (if (= 1 (count marked-boards))
;;           {:winning-board (first marked-boards) :winning-number drawn-number}
;;           (recur (remove (set winning-boards) marked-boards) (rest dns)))
;;         (recur marked-boards (rest dns))))))

(defn find-last-board [{:keys [boards draw-numbers]}]
  (loop [bs boards dns draw-numbers]
    (let [drawn-number (first dns)
          marked-boards (mark-boards bs drawn-number)
          winning-boards (get-winning-boards marked-boards)]
      (if (not-empty winning-boards)
        (if (= 1 (count marked-boards))
          {:winning-board (first marked-boards) :winning-number drawn-number}
          (recur (remove (set winning-boards) marked-boards) (rest dns)))
        (recur marked-boards (rest dns))))))

(def winning-board (get-winning-boards @board-atom))

(def last-board-map (find-last-board mapped-input))

(defn filter-unmarked [board]
  (filter (fn [{:keys [marked]}] (= marked false)) board))

(def unmarked-on-winning-board (filter-unmarked (:winning-board last-board-map)))

(defn sum-spaces [spaces]
  (reduce (fn [acc {:keys [num]}] (+ acc num)) 0 spaces))


(def sum-unmarked-last (sum-spaces 
                        (filter-unmarked (:winning-board last-board-map))))

(def sum-unmarked (sum-spaces unmarked-on-winning-board))

(def solution (* sum-unmarked-last (:winning-number last-board-map)))

;;   (def get-winning-board
;;     (let [col-state (map get-column-winners @board-atom)
;;           row-states (map get-row-winners @board-atom)]
;;       (for [i (range (count col-state))]
;;         )))

(def two-winning-board-draw [22 13 17 11 3 15 2 22 0])
(def two-winning-boards (draw-numbers-on-boards (:boards mapped-input) two-winning-board-draw))