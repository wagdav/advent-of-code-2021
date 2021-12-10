(ns aoc2021.day10
  (:require [clojure.string :as str]))

(def example-input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(defn parse-input [input]
  (->> (str/split-lines input)))

(def matching-closing {\( \) \[ \] \{ \} \< \>})

(defn balanced? [chunk]
  (reduce
    (fn [st c]
      (condp get c
        #{\( \[ \{ \<} (conj st c)
        #{\) \] \} \>} (if (= c (matching-closing (peek st)))
                           (pop st)
                           (reduced {:corrupted c}))))
    '()
    chunk))

(defn solve-part1 [input]
  (->> input
       (map balanced?)
       (keep :corrupted)
       (map {\) 3 \] 57 \} 1197 \> 25137})
       (apply +)))

(defn score [completion]
  (reduce
    (fn [total c]
      (+ (* 5 total) ({\) 1 \] 2 \} 3 \> 4} c)))
    0
    completion))

(defn solve-part2 [input]
  (let [scores (->> input
                 (map balanced?)
                 (remove :corrupted)
                 (map #(map matching-closing %))
                 (map score)
                 sort)
        midpoint (quot (dec (count scores)) 2)]
    (nth scores midpoint)))
