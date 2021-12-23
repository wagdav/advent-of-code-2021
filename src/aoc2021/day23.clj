(ns aoc2021.day23
  (:require [clojure.string :as str]))

(def example-input "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(defn parse-input [input]
  (->> (re-seq #"[A-D]" input)
       (map first)
       (partition 4)
       (apply mapv vector)
       (zipmap [\A \B \C \D])))

(def step-energy {\A 1 \B 10 \C 100 \D 1000})

(parse-input example-input)

(defn clean-room?
  "Empty room or only occupied by `a`-class  amphipods"
  [rooms a]
  (or (empty? (rooms a)) (empty? (remove #{a} (rooms a)))))

; Positions could be:
{:room \A :pos 0}

; Hallway spaces and their capacity
;
;#############
;#11.2.3.4.55#
;###.#.#.#.###
;  #.#.#.#.#
;  #########")
(def hallway {1 2
              2 1
              3 4
              5 2})

(def paths {\A [1 2]
            \B [2 3]
            \C [3 4]
            \D [4 5]})


(defn parse-input [input])


(defn solve-part1 [input])
(defn solve-part2 [input])
