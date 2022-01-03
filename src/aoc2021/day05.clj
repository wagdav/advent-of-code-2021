(ns aoc2021.day05)

(def example-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)
       (partition 4)))

(defn range-inclusive [a b]
  (if (<= a b)
    (range a (inc b) 1)
    (range a (dec b) -1)))

(defn diagonal? [[x1 y1 x2 y2]]
  (zero? (rem (Math/abs (- y1 y2)) (Math/abs (- x1 x2)))))

(defn horizontal? [[x1 _ x2 _]]
  (= x1 x2))

(defn vertical? [[_ y1 _ y2]]
  (= y1 y2))

(defn line-points [[x1 y1 x2 y2 :as line]]
  (cond
    (horizontal? line) (for [y (range-inclusive y1 y2)] [x1 y])
    (vertical? line) (for [x (range-inclusive x1 x2)] [x y1])
    :else (let [slope (/ (- y2 y1)
                         (- x2 x1))]
             (for [x (range-inclusive x1 x2)]
               [x
                (+ y1 (* slope (- x x1)))]))))

(defn count-lines [pred input]
  (->> input
    (filter pred)
    (mapcat line-points)
    frequencies
    vals
    (filter #(< 1 %))
    count))

(defn solve-part1 [input]
  (count-lines #(or (horizontal? %) (vertical? %)) input))

(defn solve-part2 [input]
  (count-lines #(or (horizontal? %) (vertical? %) (diagonal? %)) input))
