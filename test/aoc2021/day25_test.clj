(ns aoc2021.day25-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day25 :refer [example-input
                                   parse-input
                                   solve-part1]]))

(deftest works
  (testing "with example input input"
    (let [input (parse-input example-input)]
      (is (= 58 (solve-part1 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day25.txt")))]
      (is (= 278 (solve-part1 input))))))

