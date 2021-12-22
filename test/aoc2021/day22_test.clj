(ns aoc2021.day22-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day22 :refer [parse-input
                                   solve-part1
                                   solve-part2]]))

(def reboot-steps-example "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

(deftest works
  (testing "reboot-test"
    (let [input (parse-input reboot-steps-example)]
      (is (= 39 (solve-part2 input)))))

  (testing "with example input input"
    (let [input (parse-input (slurp (io/resource "day22_example.txt")))]
      (is (= 590784 (solve-part1 input))))

    (let [input (parse-input (slurp (io/resource "day22_example2.txt")))]
      (is (= 2758514936282235 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day22.txt")))]
      (is (= 588120 (solve-part1 input)))
      (is (= 1134088247046731 (solve-part2 input))))))
