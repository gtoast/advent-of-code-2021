(ns day8.part1
  (:require [clojure.string :refer [split-lines split join trim]]
            [clojure.pprint :refer [print-table pprint pp]]))

(def test-input 
"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def puzzle-input
  (slurp "day8/input.txt"))

(defn init-data [data] 
  (->> data
       split-lines
       (map #(split % #" \| "))
       (map (fn [lines]
              (mapv #(split % #" ") lines)))))

(def digits-with-unique-segments {\1 2 \4 4 \7 3 \8 7})

(defn get-solution [data]
  (let [data (init-data data)
        just-output-values (map second data)
        freq-of-lengths (frequencies (mapcat #(map count %) just-output-values))]
    (reduce-kv (fn [acc _ v] (+ acc v)) 
               0 
               (select-keys freq-of-lengths (vals digits-with-unique-segments)))))

(defn -main []
  (get-solution puzzle-input))

(comment
  
  ;split the data into vector of signal patterns 
  ;and a vector of output values
  (let [data (init-data test-input)
        just-output-values (map second data)
        output-counts (mapcat #(map count %) just-output-values)
        freq-of-lengths (frequencies (mapcat #(map count %) just-output-values))]
    ;; (pprint data)

    ;; (pprint (map second data))

    ;; (pprint (mapcat #(map count %) just-output-values)))
    ;; (apply merge-with + (mapcat frequencies just-output-values))
    ;; (reduce-kv (fn [acc _ v] (+ acc v)) 0 (select-keys freq-of-lengths (vals digits-with-unique-segments)))

    (into {} (map #([%1 (count %2)]) digits))
    )

)