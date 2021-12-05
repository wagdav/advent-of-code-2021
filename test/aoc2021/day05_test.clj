(ns aoc2021.day05-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day05 :refer [example-input parse-input line-points solve-part1 solve-part2]]))

(deftest works
  (testing "line drawing examples"
    (are
      [x y] (= x y)
      ; horizontal
      (line-points [1 1 1 3]) [[1 1] [1 2] [1 3]]
      ; vertical
      (line-points [9 7 7 7]) [[9 7] [8 7] [7 7]]
      ; diagonals
      (line-points [1 1 3 3]) '([1 1] [2 2] [3 3])
      (line-points [9 7 7 9]) '([9 7] [8 8] [7 9])))

  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 5 (solve-part1 input)))
      (is (= 12 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day05.txt")))]
      (is (= 5608 (solve-part1 input)))
      (is (= 20299 (solve-part2 input))))))
