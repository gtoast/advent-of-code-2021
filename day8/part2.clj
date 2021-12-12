(ns day8.part2
  (:require [clojure.string :refer [split-lines split join trim]]
            [clojure.pprint :refer [print-table pprint pp]]
            [clojure.set :refer [difference union intersection]]))

(def digits-with-unique-segments-counts {\1 2 \4 4 \7 3 \8 7})

(def digits
  {\0 #{:t :ur :ul :br :bl :b}
   \1 #{:ur :br}
   \2 #{:t :ur :m :bl :b}
   \3 #{:t :ur :m :br :b}
   \4 #{:ul :m :ur :br}
   \5 #{:t :ul :m :br :b}
   \6 #{:t :ul :m :br :b :bl}
   \7 #{:t :ur :br}
   \8 #{:t :ur :m :ul :br :b :bl}
   \9 #{:t :ur :m :ul :br :b}})

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
       (mapv (fn [lines]
               (map #(split % #" ") lines)))
       (mapv (fn [lines] (mapv (fn [sect] (mapv set sect)) lines)))))


(defn create-signal-map [initialized-signal-data]
   (let [map-signal-lengths (map #(vector (count %) %) initialized-signal-data)
         one   (second (first (filter #(= 2 (first %)) map-signal-lengths)))
         four  (second (first (filter #(= 4 (first %)) map-signal-lengths)))
         seven (second (first (filter #(= 3 (first %)) map-signal-lengths)))
         eight (second (first (filter #(= 7 (first %)) map-signal-lengths)))
         five-segment-signals (map second (filter #(= 5 (first %)) map-signal-lengths))
         ul-m (difference four one) ;the upper-left and middle segments
         five-segment-signals-inter-ul-m (map (juxt #(count (intersection % ul-m)) identity) five-segment-signals)
         five (second (first (filter #(= 2 (first %)) five-segment-signals-inter-ul-m))) ;the five segment with 2 segments after union with ul-m is the "5" signal
         br (intersection five one) ;the bottom-right segment
         remaining-five-segment-signals-inter-br (map (juxt #(intersection br %) identity) (remove #(= % five) five-segment-signals))
         three (second (first (filter #(= br (first %)) remaining-five-segment-signals-inter-br))) ;the remaining five-segment signal with a br segment is the "3" signal
         two (first (filter (fn [sig] (and (not= three sig) (not= five sig))) five-segment-signals)) ;the last five-segment signal id'd is the "2" signal
         nine (union three four) ; three + four makes nine =P
         six (union five (difference eight nine))
         zero  (first (filter #(and (not= nine %) (not= six %)) (map second (filter #(= 6 (first %)) map-signal-lengths))))]
     {zero "0" , one "1", two "2", three "3", four "4", five "5", six "6", seven "7", eight "8", nine "9"}))


(defn convert-output-to-num [signal-map four-digit-output]
  (join (map signal-map four-digit-output)))


(defn get-solution [data]
  (let [data (init-data data)
        display-strings (map (fn [[signal-data four-digit-output]] 
                               (let [signal-map (create-signal-map signal-data)] 
                                 (convert-output-to-num signal-map four-digit-output))) 
                             data)]
    (reduce (fn [acc s] (+ acc (Integer/parseInt s))) 0 display-strings)))

(defn -main []
  (get-solution puzzle-input))

(comment
  
;how many segments are in each digit
(group-by second (map #(vector (first %) (count (second %))) digits))
;; => {6 [[\0 6] [\6 6] [\9 6]], 2 [[\1 2]], 5 [[\2 5] [\3 5] [\5 5]], 4 [[\4 4]], 3 [[\7 3]], 7 [[\8 7]]}

; (intersection (digits \9) (digits \1))

; (intersection (digits \6) (digits \1))
  
(def ul-m (difference 'four 'one)) 



  ;split the data into vector of signal patterns 
  ;and a vector of output values
  (let [data (init-data test-input)]
    

    (let [[signal-data four-digit-output] (data 7)
          signal-map (create-signal-map signal-data)]
      ;; (data 0)
      ;; signal-map
      (convert-output-to-num signal-map four-digit-output)
      ))


    ;(pprint data)

    ;(pprint (map second data))

    ;(pprint (mapcat #(map count %) just-output-values)))
    ;; (apply merge-with + (mapcat frequencies just-output-values))
    ;; (reduce-kv (fn [acc _ v] (+ acc v)) 0 (select-keys freq-of-lengths (vals digits-with-unique-segments))))


    ;; (let [data (get-in data [7 0])
    ;;       map-signal-lengths (map #(vector (count %) %) data)
    ;;       one   (second (first (filter #(= 2 (first %)) map-signal-lengths)))
    ;;       four  (second (first (filter #(= 4 (first %)) map-signal-lengths)))
    ;;       seven (second (first (filter #(= 3 (first %)) map-signal-lengths)))
    ;;       eight (second (first (filter #(= 7 (first %)) map-signal-lengths)))
    ;;       five-segment-signals (map second (filter #(= 5 (first %)) map-signal-lengths))
    ;;       ul-m (difference four one) ;the upper-left and middle segments
    ;;       five-segment-signals-inter-ul-m (map (juxt #(count (intersection % ul-m)) identity) five-segment-signals)
    ;;       five (second (first (filter #(= 2 (first %)) five-segment-signals-inter-ul-m))) ;the five segment with 2 segments after union with ul-m is the "5" signal
    ;;       br (intersection five one) ;the bottom-right segment
    ;;       remaining-five-segment-signals-inter-br (map (juxt #(intersection br %) identity) (remove #(= % five) five-segment-signals)) 
    ;;       three (second (first (filter #(= br (first %)) remaining-five-segment-signals-inter-br))) ;the remaining five-segment signal with a br segment is the "3" signal
    ;;       two (first (filter (and #(not= three %) #(not= five %)) five-segment-signals)) ;the last five-segment signal id'd is the "2" signal
    ;;       nine (union three four) ; three + four makes nine =P
    ;;       six (union five (difference eight nine))
    ;;       zero  (first (filter #(and (not= nine %) (not= six %)) (map second (filter #(= 6 (first %)) map-signal-lengths))))]
    ;; ;; (into {} (map #(vector (first %1) (count (second %1))) digits))
    ;; ;; (map count (get-in data [0 0])))
    ;; ;; five-segment-signals
    ;; ;; two ;; => #{\a \b \c \d \f})
    ;; ;; three ;; => #{\b \c \d \e \f}
    ;; ;; five ;; => #{\c \d \e \f \g}
    ;; {"0" zero, "1" one, "2" two, "3" three, "4" four, "5" five, "6" six, "7" seven, "8" eight, "9" nine}
    ;; )))
 
)