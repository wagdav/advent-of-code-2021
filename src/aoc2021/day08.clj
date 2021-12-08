(ns aoc2021.day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(re-seq #"\w+" %))
       (map #(partition-all 10 %))))

(defn count-easy-digits [output-values]
  (->> output-values
       (map count)
       (keep #{2 4 3 7})
       count))

(defn solve-part1 [input]
  (->> input
       (map second)
       (map count-easy-digits)
       (apply +)))

(defn combine [a b]
  (set/union (set a) (set b)))

(defn find-with-mask
  "Find p in pattern such that

      p ⊕ mask = p, or
      p ⊕ mask = target

  where ⊕ means overlaying the two digit patterns"
  ([mask patterns]
   (first (filter #(= (combine % mask) (set %)) patterns)))

  ([mask target patterns]
   (first (filter #(= (combine % mask) (set target)) patterns))))

(defn decode [patterns]
  (let [group  (group-by count patterns)
        ; Unambiguous digits
        one    (first (get group 2))
        four   (first (get group 4))
        seven  (first (get group 3))
        eight  (first (get group 7))
        ; Candiates for 2,3,5 and 0,6,9
        c235   (get group 5)
        c069   (get group 6)
        ; Find the rest by exploiting their similarities in shape
        three  (find-with-mask one c235)
        nine   (find-with-mask three c069)
        zero   (->> c069
                    (remove #{nine})
                    (find-with-mask seven))
        six    (->> c069
                    (remove #{zero nine})
                    first)
        five   (->> c235
                    (remove #{three})
                    (find-with-mask nine nine))
        two    (first (remove #{three five} c235))]
    {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}))

(defn decode-line [[patterns output]]
  (let [m (into {} (map (fn [[k v]] [(set k) v])
                        (decode patterns)))]
    (->> output
         (map (comp m set))
         (apply str)
         (Integer/parseInt))))

(defn solve-part2 [input]
  (apply + (map decode-line input)))
