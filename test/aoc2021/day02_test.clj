(ns aoc2021.day02-test
  (:require [clojure.test :refer [deftest is testing]]
            [aoc2021.day02 :refer [example parse-input solve-part1 solve-part2]]
            [clojure.java.io :as io]))

(deftest works
  (testing "with example input"
    (let [input (parse-input example)]
      (is (= 150 (solve-part1 input)))
      (is (= 900 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day02.txt")))]
      (is (= 1693300 (solve-part1 input)))
      (is (= 1857958050 (solve-part2 input))))))
