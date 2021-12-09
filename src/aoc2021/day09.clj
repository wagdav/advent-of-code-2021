(ns aoc2021.day09
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(re-seq #"\w" %))
       (mapv #(mapv read-string %))))

(defn adjacent-coords [[x y]]
  (for [coord [[x y]
               [x (dec y)]
               [x (inc y)]
               [(dec x) y]
               [(inc x) y]]]
   coord))

(defn adjacent [rows point]
  (->> (for [coord (adjacent-coords point)] (get-in rows coord))
       (remove nil?)))

(defn adjacent-all [rows point]
  (->> (adjacent-coords point)
       (map (fn [p] [p (get-in rows p)]))
       (remove #(nil? (second %)))))

(defn low? [points]
  (let [p (first points)
        r (rest points)]
    (every? #(< p %) r)))

(defn low-points [input]
  (let [nrows (count input)
        ncols (count (first input))]
    (for [x (range nrows)
          y (range ncols)
          :when (low? (adjacent input [x y]))]
      [x y])))

(defn basin [position rows]
  (loop [queue (vector position)
         visited #{}
         basin #{}]
    (if-not (seq queue)
      basin
      (let [cur       (first queue)
            adjacent  (rest (adjacent-all rows cur))
            more?     (< (get-in rows cur) 9)
            to-queue  (if more? (remove visited (map first adjacent)) [])]
        (recur
          (concat (rest queue) to-queue)
          (conj visited cur)
          (if more? (conj basin cur) basin))))))

(defn solve-part1 [input]
  (->> (low-points input)
       (map #(get-in input %))
       (map inc)
       (apply +)))

(defn solve-part2 [input]
  (->> (low-points input)
       (map #(basin % input))
       (map count)
       (sort >)
       (take 3)
       (apply *)))

