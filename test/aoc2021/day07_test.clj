(ns aoc2021.day07-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day07 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 37 (solve-part1 input)))
      (is (= 168 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day07.txt")))]
      (is (= 335330 (solve-part1 input)))
      (is (= 92439766 (solve-part2 input))))))

