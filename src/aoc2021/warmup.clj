; As a warmup I implemented https://adventofcode.com/2020/day/1 in Clojure
; Some tricks come from this video: https://www.youtube.com/watch?v=9ITiZ88sljA
(ns aoc2021.warmup
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]))

(def example [1721
              979
              366
              299
              675
              1456])

(def input
  (map #(Integer/parseInt %)
       (line-seq (io/reader (io/resource "warmup.txt")))))

(defn solve-part1
  [input]
  (first
    (set
      (for [x input
            y input
            :when (= 2020 (+ x y))]
       (* x y)))))

(defn solve-part2
  [input]
  (first
    (set
      (for [x input
            y input
            z input
            :when (= 2020 (+ x y z))]
       (* x y z)))))

(deftest tests
  (testing "Solves Part 1"
    (is (= (solve-part1 example) 514579))
    (is (= (solve-part1 input) 927684)))
  (testing "Solves Part 2"
    (is (= (solve-part2 example) 241861950))
    (is (= (solve-part2 input) 292093004))))
