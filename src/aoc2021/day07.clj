(ns aoc2021.day07)

(def example-input "16,1,2,0,4,2,7,1,2,14")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)))

(defn abs [n] (max n (- n)))

(defn distance [x y] (abs (- x y)))

(defn distance2 [x y]
  (reduce + (range 1 (inc (distance x y)))))

(defn cost [distance positions target]
  (reduce + (map #(distance % target) positions)))

(defn solve-part1 [input]
  (apply min
    (map
      #(cost distance input %)
      input)))

(defn solve-part2 [input]
  (apply min
    (map
      #(cost distance2 input %)
      input)))
