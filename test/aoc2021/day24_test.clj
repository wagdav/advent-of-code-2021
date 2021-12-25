(ns aoc2021.day24-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day24 :refer [parse-input
                                   solve-part1
                                   solve-part2]]))

(deftest works
  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day24.txt")))]
      (is (= 99799212949967 (solve-part1 input)))
      (is (= 34198111816311 (solve-part2 input))))))
