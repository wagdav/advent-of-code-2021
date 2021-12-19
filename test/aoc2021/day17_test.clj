(ns aoc2021.day17-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day17 :refer [example-input
                                   parse-input
                                   solve-part1
                                   solve-part2]]))

(deftest working
  (testing "with example input input"
    (let [input (parse-input example-input)]
      (is (= 45 (solve-part1 input)))
      (is (= 112 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day17.txt")))]
      (is (= 4095 (solve-part1 input)))
      (is (= 3773 (solve-part2 input))))))

