(ns aoc2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input [199 200 208 210 200 207 240 269 260 263])

(defn parse-input [input-file]
  (->> (io/resource input-file)
       slurp
       str/split-lines
       (mapv #(Integer/parseInt %))))

(defn solve-part1 [input]
  (->> (partition 2 1 input)
       (filter #(apply < %))
       count))

(defn solve-part2 [input]
  (->> (partition 3 1 input)
       (map #(apply + %))
       solve-part1))
