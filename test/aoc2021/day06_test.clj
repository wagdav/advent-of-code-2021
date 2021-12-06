(ns aoc2021.day06-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day06 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 5934 (solve-part1 input)))
      (is (= 26984457539 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day06.txt")))]
      (is (= 350149 (solve-part1 input)))
      (is (= 1590327954513 (solve-part2 input))))))

