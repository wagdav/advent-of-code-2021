(ns aoc2021.day08-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day08 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 26 (solve-part1 input)))
      (is (= 61229 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day08.txt")))]
      (is (= 245 (solve-part1 input)))
      (is (= 983026 (solve-part2 input))))))


