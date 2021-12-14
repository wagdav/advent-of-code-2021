(ns aoc2021.day14-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day14 :refer [parse-input solve-part1 solve-part2]]))

(def example-input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(deftest works
  (testing "with example input"
    (let [input (parse-input example-input)]
      (is (= 1588 (solve-part1 input)))
      (is (= 2188189693529 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day14.txt")))]
      (is (= 2360 (solve-part1 input)))
      (is (= 2967977072188 (solve-part2 input))))))
