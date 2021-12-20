(ns aoc2021.day19-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day19 :refer [parse-input
                                   solve-part1
                                   solve-part2]]))

(deftest works
  (testing "with example input input"
    (let [input (parse-input (slurp (io/resource "day19_example.txt")))]
      (is (= 79 (solve-part1 input)))
      (is (= 3621 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day19.txt")))]
      (is (= 432 (solve-part1 input)))
      (is (= 14414 (solve-part2 input))))))

