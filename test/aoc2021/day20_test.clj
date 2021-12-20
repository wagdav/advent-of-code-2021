(ns aoc2021.day20-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day20 :refer [parse-input
                                   solve-part1
                                   solve-part2]]))

(deftest works
  (testing "with example input input"
    (let [input (parse-input (slurp (io/resource "day20_example.txt")))]
      (is (= 35 (solve-part1 input)))
      (is (= 3351 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day20.txt")))]
      (is (= 5573 (solve-part1 input)))
      (is (= 20097 (solve-part2 input))))))

