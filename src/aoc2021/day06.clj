(ns aoc2021.day06)

(def example-input "3,4,3,1,2")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)))

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
        (recur (-> (into {} (for [i (range 9)]
                                 [i
                                  (get fish (mod (inc i) 9) 0)]))
                   (update 6 + (get fish 0 0)))
               (dec t)))))

(defn solve-part2 [input]
  (fish 256 input))
