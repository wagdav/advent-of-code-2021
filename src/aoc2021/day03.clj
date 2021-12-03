(ns aoc2021.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example-input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn transpose [m]
  (apply mapv vector m))

(defn most-common [m]
  (let [f (frequencies m)]
   (if (<= (f \0) (f \1)) \1 \0)))

(defn least-common [m]
  (let [f (frequencies m)]
   (if (<= (f \0) (f \1)) \0 \1)))

(defn parse-input [input]
  (str/split-lines input))

(defn rate-with [f input]
  (->> (transpose input)
       (map f)
       (apply str "2r")
       (read-string)))

(defn gamma-rate [input]
  (rate-with most-common input))

(defn epsilon-rate [input]
  (rate-with least-common input))

(defn solve-part1 [input]
  (let [g (gamma-rate input)
        e (epsilon-rate input)] (* e g)))

(solve-part1 (parse-input example-input))

(solve-part1 (parse-input (slurp (io/resource "day03.txt"))))

;================

(defn apply-mask [vs mask]
  (map first
    (filter (fn [[_ b]] (true? b))
      (map (fn [a b] [a b]) vs mask))))

(defn rating [f]
  (fn [cols]
    (reduce
      (fn [mask col]
        (if (= ((frequencies mask) true) 1)
            (reduced mask)
            (map (fn [m c] (and (true? m) (= c (f (apply-mask col mask))))) mask col)))
      (repeat (count (first cols)) true)
      cols)))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn to-decimal [s]
  (read-string (str "2r" s)))

(defn solve-part2 [input]
  (let [cols (transpose input)
        o2 (first (indices true? ((rating most-common) cols)))
        co2 (first (indices true? ((rating least-common) cols)))]
   (*
    (to-decimal (nth input o2))
    (to-decimal (nth input co2)))))

(solve-part2 (parse-input example-input)) ; should be 230
(solve-part2 (parse-input (slurp (io/resource "day03.txt"))))
