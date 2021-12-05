(ns aoc2021.day02
  (:require [clojure.string :as str]))

(def example "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(str/split % #" "))
       (mapv (fn [[d v]] [d (read-string v)]))))

(defn compute-result [{:keys [pos depth]}]
  (* pos depth))

(defn solve-part1 [input]
  (->> input
       (reduce
         (fn [state [command amount]]
           (case command
             "forward" (update state :pos + amount)
             "down" (update state :depth + amount)
             "up" (update state :depth - amount)))
         {:pos 0 :depth 0})
       compute-result))

(defn solve-part2 [input]
  (->> input
       (reduce
         (fn [{:keys [pos depth aim] :as state} [command amount]]
           (case command
             "down" (update state :aim + amount)
             "up" (update state :aim - amount)
             "forward" (assoc state :pos (+ amount pos)
                                    :depth (+ depth (* aim amount)))))
         {:pos 0 :depth 0 :aim 0})
       compute-result))
