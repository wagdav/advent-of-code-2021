(ns aoc2021.day12-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day12 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 10 (solve-part1 input)))
      (is (= 36 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day12.txt")))]
      (is (= 4549 (solve-part1 input)))
      (is (= 120535 (solve-part2 input))))))
