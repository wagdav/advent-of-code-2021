(ns aoc2021.day09
  (:require [clojure.string :as str]))

(def example-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(re-seq #"\w" %))
       (mapv #(mapv read-string %))))

(defn neigbours [rows [x y]]
  (let [xmax (-> rows count dec)
        ymax (-> rows first count dec)]
    (cond-> []
      (pos?   x) (conj [(dec x)      y])
      (< x xmax) (conj [(inc x)      y])
      (pos?   y) (conj [     x  (dec y)])
      (< y ymax) (conj [     x  (inc y)]))))

(defn low? [point neigbours]
  (every? #(< point %) neigbours))

(defn low-points [input]
  (let [nrows  (count input)
        ncols  (count (first input))
        height (partial get-in input)
        near   (partial neigbours input)]
    (for [x (range nrows) y (range ncols)
          :when (low? (height [x y])
                      (map height (near [x y])))]
      [x y])))

(defn basin [position rows]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY position)
         basin #{}]
    (if (empty? queue)
      basin
      (let [cur       (peek queue)
            adjacent  (neigbours rows cur)
            more?     (< (get-in rows cur) 9)
            to-queue  (if more? (remove basin adjacent) [])]
        (recur
          (into (pop queue) to-queue)
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

