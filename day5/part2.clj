(ns day5.part2
  (:require [clojure.string :refer [split-lines split join trim]]
            [clojure.pprint :refer [print-table pprint pp]]))

(def raw-test-input
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def raw-input
  (slurp "day5/input.txt"))

(defn coerce-input [input]
  (as-> input inp
    (split inp #"->|,|\n")
    (map trim inp)
    (map #(Integer/parseInt %) inp)))

(defn vents-as-coord-pairs [input]
  (partition 4 input))

(defn is-horizontal-vertical? [x1 y1 x2 y2]
  (or (= x1 x2) (= y1 y2)))

(defn horz-vert-lines-to-points [x1 y1 x2 y2]
  (if (= x1 x2)
    (for [y (range (min y1 y2) (inc (max y1 y2))) :let [x x1]]
      [x y])
    (for [x (range (min x1 x2) (inc (max x1 x2))) :let [y y1]]
      [x y])))

(defn diag-lines-to-points [x1 y1 x2 y2]
  (let [x-step (if (pos? (- x1 x2)) -1 1)
        y-step (if (pos? (- y1 y2)) -1 1)
        x-inclu (if (pos? x-step) inc dec)
        y-inclu (if (pos? y-step) inc dec)]
    (mapv vector (range x1 (x-inclu x2) x-step) (range y1 (y-inclu y2) y-step))))

(defn field-as-rows
  ([{:keys [field max-x] :as input}]
   (partition max-x field))
  ([field column-count]
   (partition column-count field)))

(defn all-xs [input]
  (apply concat (partition 1 2 input)))

(defn all-ys [input]
  (apply concat (partition 1 2 (drop 1 input))))

(defn inc-location 
  ([field [row col] column-size]
    (inc-location field row col column-size))
  ([field row col column-size]
    (update field (+  row (* column-size col)) inc)))

(defn make-field [row col]
  (vec (for [_ (range (* row col))] 0)))

(defn solution [input]
  (let [input (coerce-input input)
        max-x (inc (apply max (all-xs input)))
        max-y (inc (apply max (all-ys input)))
        field (make-field max-x max-y)
        ;build horizontal/vertical points
        horiz-vert-coord-pairs (filter #(apply is-horizontal-vertical? %) (vents-as-coord-pairs input))
        horiz-vert-vent-points (mapcat #(apply horz-vert-lines-to-points %) horiz-vert-coord-pairs)
        ;build diagonal poitns
        diag-coord-pairs (remove #(apply is-horizontal-vertical? %) (vents-as-coord-pairs input))
        diag-vent-points (mapcat #(apply diag-lines-to-points %) diag-coord-pairs)
        incremented-field (reduce #(inc-location %1 %2 max-x) field (concat diag-vent-points horiz-vert-vent-points)) ]
   (count (filter #(>= % 2) incremented-field))))


  ;find the solution
  (solution raw-input)
  ;; => 17717

(comment

  ;;view the incremented field as rows
  (pprint (field-as-rows incremented-field max-x))
  
  ;;diag lines from the test data => ((8 0 0 8) (6 4 2 0) (0 0 8 8) (5 5 8 2))
  ;;check diagonal points creation
  (apply diag-lines-to-points '(5 5 8 2))

  ;increment the point 3,2 and view the field as sequence of lines
  (field-as-rows (inc-location (:field input) 3 2 (:max-y input)) (:max-x input))

  ;get the first coordinante pair from the vent data and get the points that need to be incremented
  (apply horz-vert-lines-to-points (first (vents-as-coord-pairs (:vent-lines data))))
  
  ;remove all the non-horizontal/vertical lines from the vent data
  (filter #(apply is-horizontal-vertical? %) (vents-as-coord-pairs (:vent-lines data)))
  
  ;show field as rows of max-x points
  (field-as-rows incremented-field max-x)

  ;line endings next to their created points
  (zipmap horiz-vert-coord-pairs horiz-vert-vent-points)

  ;count the points greater than or equal to 2
  (count (filter #(>= % 2) incremented-field))
  )

