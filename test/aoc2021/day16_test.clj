(ns aoc2021.day16-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.java.io :as io]
            [aoc2021.day16 :refer [eval-packet
                                   parse-input
                                   total-version
                                   solve-part1
                                   solve-part2]]))

(deftest works
  (testing "packet parsing examples"
    (are
      [packet output] (= output (parse-input packet))

      "D2FE28"
      {:id 4
       :version 6
       :type :literal
       :value 2021
       :input '(0 0 0)}

      "38006F45291200"
      {:id 6
       :version 1,
       :type :lt
       :input `(0 0 0 0 0 0 0),
       :length-type 0
       :operands
         [{:id 4
           :version 6
           :type :literal
           :value 10}

          {:id 4
           :version 2,
           :type :literal,
           :value 20}]}))

  (testing "version count examples"
    (are
      [value packet] (= value (total-version (parse-input packet)))

      16 "8A004A801A8002F478"
      12 "620080001611562C8802118E34"
      23 "C0015000016115A2E0802F182340"
      31 "A0016C880162017C3686B18A3D4780"))

  (testing "packet eval"
    (are
      [value packet] (= value (eval-packet (parse-input packet)))

      3  "C200B40A82"
      54 "04005AC33890"
      7  "880086C3E88112"
      9  "CE00C43D881120"
      1  "D8005AC2A8F0"
      0  "F600BC2D8F"
      0  "9C005AC2F8F0"
      1  "9C0141080250320F1802104A08"))

  (testing "with real input"
    (let [input (slurp (io/resource "day16.txt"))]
      (is (= 960 (solve-part1 input)))
      (is (= 12301926782560 (solve-part2 input))))))
