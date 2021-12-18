(ns aoc2021.day09-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day09 :refer [example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 15 (solve-part1 input)))
      (is (= 1134 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day09.txt")))]
      (is (= 522 (solve-part1 input)))
      (is (= 916688 (solve-part2 input))))))
