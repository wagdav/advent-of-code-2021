(ns aoc2021.day21-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day21 :refer [parse-input
                                   solve-part1
                                   solve-part2]]))

(deftest works
  (testing "with example input input"
    (let [input [4 8]]
      (is (= 739785 (solve-part1 input)))
      (is (= 444356092776315 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day21.txt")))]
      (is (= 518418 (solve-part1 input)))
      (is (= 116741133558209 (solve-part2 input))))))
