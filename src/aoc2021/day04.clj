(ns aoc2021.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

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
 2  0 12  3  7")

(defn parse-input [input]
  (let [blocks (str/split input #"\R\R")]
    {:numbers
     (mapv read-string (re-seq #"\d+" (first blocks)))
     :boards
     (map
       (fn [board-lines]
         (mapv
           #(mapv read-string (re-seq #"\d+" %))
           (str/split-lines board-lines)))
       (rest blocks))}))

(defn transpose [m]
  (apply mapv vector m))

(defn wins?
  "The board wins if it has at least one complete row or column of marked numbers"
 [board marked]
 (let [all-marked? (fn [nums] (every? (set marked) nums))]
   (if (or
         (some all-marked? board)
         (some all-marked? (transpose board)))
     board
     nil)))

(defn score
  "Sum of all the unmarked numbers multiplied by the last drawn number"
  [board marked]
  (*
    (->> (flatten board)
         (remove (set marked))
         (reduce +))
    (first marked)))

(= 4512 (score (nth (example-input :boards) 2)
               '(24 21 14 0 2 23 17 11 5 9 4 7)))

(defn solve-part1 [{:keys [numbers boards]}]
  (reduce
    (fn [marked n]
     (let [numbers (conj marked n)
           winning-board (some #(wins? % numbers) boards)]
       (if winning-board
         (reduced (score winning-board numbers))
         numbers)))
    ()
    numbers))

(defn game [{:keys [numbers boards]}]
  (reductions
    (fn [{:keys [drawn winners in-play scores] :as state} n]
       (let [nums   (conj drawn n)
             winner (first (keep #(wins? % nums) in-play))]
         (cond-> state
           true   (assoc :drawn nums)
           winner (assoc
                    :scores (conj scores (score winner nums))
                    :in-play (disj in-play winner)
                    :winner-count (keep #(wins? % nums) in-play)))))
    {:drawn '()
     :in-play (set boards)
     :scores '()}
    numbers))

(defn solve-part2 [input]
  (first (:scores (game input))))

(comment
  (game example-input)

  (game real-input)

  (solve-part2 example-input)
  (solve-part2 real-input)

  (def example-input (parse-input example))
  (def real-input (parse-input (slurp (io/resource "day04.txt"))))

  (= 4512 (solve-part1 example-input))

  (= 31424 (solve-part1 real-input))
  (= 23042 (solve-part2 real-input)))
