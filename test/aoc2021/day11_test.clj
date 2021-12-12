(ns aoc2021.day11-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day11 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
 (testing "with example input"
   (let [input (parse-input example-input)]
     (is (= 1656 (solve-part1 input)))
     (is (= 195 (solve-part2 input)))))

 (testing "with real input"
   (let [input (parse-input (slurp (io/resource "day11.txt")))]
     (is (= 1683 (solve-part1 input)))
     (is (= 788 (solve-part2 input))))))
