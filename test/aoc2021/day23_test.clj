(ns aoc2021.day23-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day23 :refer [example-input
                                   parse-input
                                   solve-part1
                                   solve-part2]]))

(deftest works
  (testing "with example input input"
    (let [input (parse-input example-input)]
      (is (= 12521 (solve-part1 input)))
      #_(is (= 0 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day23.txt")))]
      (is (= 0 (solve-part1 input)))
      #_(is (= 0 (solve-part2 input))))))

