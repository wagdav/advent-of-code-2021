(ns aoc2021.day25
  (:require [clojure.string :as str]))

(def example-input "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(apply vector %))))

(defn move-right [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (reduce
      (fn [state [x y]]
        (let [right [x (-> y inc (mod cols))]]
          (if (and (= \> (get-in grid [x y]))
                   (= \. (get-in grid right)))
            (-> state
                (assoc-in [x y] \.)
                (assoc-in right \>))
            state)))
      grid
      (for [x (range rows) y (range cols)] [x y]))))

(defn move-down [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (reduce
      (fn [state [x y]]
        (let [down [(-> x inc (mod rows)) y]]
          (if (and (= \v (get-in grid [x y]))
                   (= \. (get-in grid down)))
            (-> state
                (assoc-in [x y] \.)
                (assoc-in down \v))
            state)))
      grid
      (for [x (range rows) y (range cols)] [x y]))))

(defn step [grid]
  (-> grid move-right move-down))

(defn solve-part1 [input]
  (loop [i 1 state input]
    (let [new-state (step state)]
      (if (= new-state state)
        i
        (recur (inc i) new-state)))))
