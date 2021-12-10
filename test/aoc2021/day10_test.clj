(ns aoc2021.day10-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day10 :refer [balanced? example-input parse-input solve-part1 solve-part2]]))

(deftest works
  (testing "balanced?"
   ; Expected ], but found } instead
   (is (= {:corrupted \}} (balanced? "{([(<{}[<>[]}>{[]{[(<()>"))))

 (testing "with example input"
   (let [input (parse-input example-input)]
     (is (= 26397 (solve-part1 input)))
     (is (= 288957 (solve-part2 input)))))

 (testing "with real input"
   (let [input (parse-input (slurp (io/resource "day10.txt")))]
     (is (= 358737 (solve-part1 input)))
     (is (= 4329504793 (solve-part2 input))))))




