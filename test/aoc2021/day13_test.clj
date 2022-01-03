(ns aoc2021.day13-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day13 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 17 (solve-part1 input)))
      (is (nil? (solve-part2 input))))) ; prints a rectangle

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day13.txt")))]
      (is (= 788 (solve-part1 input)))
      (is (nil? (solve-part2 input)))))) ; prints KJBKEUBG
