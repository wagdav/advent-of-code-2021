(ns aoc2021.day15-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day15 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 40 (solve-part1 input)))
      (is (= 315 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day15.txt")))]
      (is (= 714 (solve-part1 input)))
      (is (= 2948 (solve-part2 input))))))
