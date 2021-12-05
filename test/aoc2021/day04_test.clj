(ns aoc2021.day05-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day04 :refer [example parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example)]
      (is (= 4512 (solve-part1 input)))
      (is (= 1924 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day04.txt")))]
      (is (= 31424 (solve-part1 input)))
      (is (= 20299 (solve-part2 input))))))

