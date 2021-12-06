(ns aoc2021.day06)

(def example-input "3,4,3,1,2")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)))


(->> (parse-input example-input))

(defn solve-part1 [input]
   (loop [f input
          t 80]
     (if (= t 0)
       (count f)
       (recur (mapcat (fn [f] (if (= 0 f) [6 8] [(dec f)])) f)
              (dec t)))))

(defn fish [n input]
   (loop [fish (frequencies input)
          t    n]
     (if (= t 0)
        (reduce + (vals fish))
        (recur (-> fish
                   (assoc 0 (get fish 1 0)
                          1 (get fish 2 0)
                          2 (get fish 3 0)
                          3 (get fish 4 0)
                          4 (get fish 5 0)
                          5 (get fish 6 0)
                          6 (get fish 7 0)
                          7 (get fish 8 0)
                          8 (get fish 0 0))
                   (update 6 + (get fish 0 0)))
               (dec t)))))

(defn solve-part2 [input]
  (fish 256 input))
