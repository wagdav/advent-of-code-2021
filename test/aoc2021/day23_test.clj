(ns aoc2021.day23-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day23 :refer [clean-room?
                                   example-input
                                   locations
                                   from-room
                                   parse-input
                                   solve-part1
                                   solve-part2]]))

(def example-state
  [[:room 2 1 \B]
   [:room 2 2 \A]
   [:room 4 1 \C]
   [:room 4 2 \D]
   [:room 6 1 \B]
   [:room 6 2 \C]
   [:room 8 1 \D]
   [:room 8 2 \A]])

(deftest works
  (testing "clean-room"
    (is (= true (clean-room? {2 {}} \A)))
    (is (= true (clean-room? {2 {2 \A}} \A)))
    (is (= false (clean-room? {2 {2 \B}} \A))))

  (testing "from-room"
    (are [coord rooms hallway end] (= end (from-room coord rooms hallway \A))

      ; blocked in room
      [2 2] {2 {1 \A}} {}
      #{}

      ; empty hallway
      [4 1] {2 {} 4 {} 6 {} 8 {}} {}
      #{[:hallway 0 \A] [:hallway 1 \A] [:hallway 3 \A] [:hallway 5 \A] [:hallway 7 \A] [:hallway 9 \A] [:hallway 10 \A] [:room [2 2] \A]}

      ; Position 3 in hallway is blocked
      [2 1] {2 {} 4 {} 6 {} 8 {}} {3 \A}
      #{[:hallway 0 \A] [:hallway 1 \A]}))

  (testing "with example input input"
    (let [input (parse-input example-input)]
      (is (= 12521 (solve-part1 input)))
      #_(is (= 0 (solve-part2 input)))))

  (testing "with real input"
    (let [input (parse-input (slurp (io/resource "day23.txt")))]
      (is (= 0 (solve-part1 input)))
      #_(is (= 0 (solve-part2 input))))))

