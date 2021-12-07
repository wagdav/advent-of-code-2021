(ns aoc2021.day07)

(def example-input "16,1,2,0,4,2,7,1,2,14")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)))

(defn abs [n] (max n (- n)))

(defn distance1 [x y] (abs (- x y)))

(defn distance2 [x y]
  (apply + (range 1 (inc (distance1 x y)))))

(defn cost [d positions target]
  (apply + (map #(d % target) positions)))

(defn optimize [input cost-fun]
  (let [minpos (apply min input)
        maxpos (apply max input)]
    (apply min
      (map
        cost-fun
        (range minpos (inc maxpos))))))

(defn solve-part1 [input]
  (optimize input #(cost distance1 input %)))

(defn solve-part2 [input]
  (optimize input #(cost distance2 input %)))
