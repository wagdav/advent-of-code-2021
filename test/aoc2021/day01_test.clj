(ns aoc2021.day01-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2021.day01 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (is (= 7 (solve-part1 example-input)))
    (is (= 5 (solve-part2 example-input))))

  (testing "with real input"
    (let [input (parse-input "day01.txt")]
      (is (= 1233 (solve-part1 input)))
      (is (= 1275 (solve-part2 input))))))

