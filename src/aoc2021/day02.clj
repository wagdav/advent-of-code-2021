(ns aoc2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse-input-file [input-file]
  (->> (io/resource input-file)
       slurp
       parse-input))

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(str/split % #" "))
       (mapv (fn [[d v]] [d (read-string v)]))))

(defn solve-part1 [input]
  (->> input
    (reduce
      (fn [acc [command, amount]]
        (case command
          "forward" (update acc :pos + amount)
          "down" (update acc :depth + amount)
          "up" (update acc :depth - amount)))
      {:pos 0 :depth 0})
    vals
    (reduce *)))

(defn solve-part2 [input]
  (reduce *
    (vals
      (dissoc
        (->> input
          (reduce
            (fn [acc [command, amount]]
              (case command
                "down" (update acc :aim + amount)
                "up" (update acc :aim - amount)
                "forward" (assoc acc :pos (+ amount (:pos acc))
                                     :depth (+ (:depth acc) (* (:aim acc) amount)))))
            {:pos 0 :depth 0 :aim 0}))
        :aim))))

(solve-part1 (parse-input-file "day02.txt"))

(solve-part1 (parse-input example-input))

(solve-part2 (parse-input example-input))

(solve-part2 (parse-input-file "day02.txt"))
