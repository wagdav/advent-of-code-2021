; As a warmup I implemented https://adventofcode.com/2020/day/1 in Clojure
; Some tricks come from this video: https://www.youtube.com/watch?v=9ITiZ88sljA
(ns aoc2021.warmup
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))

(def example [1721
              979
              366
              299
              675
              1456])

(def input
  (map #(Integer/parseInt %)
       (line-seq (io/reader (io/resource "warmup.txt")))))

(defn solve-part1
  [input]
  (first
    (set
      (for [x input
            y input
            :when (= 2020 (+ x y))]
       (* x y)))))

(defn solve-part2
  [input]
  (first
    (set
      (for [x input
            y input
            z input
            :when (= 2020 (+ x y z))]
       (* x y z)))))

(deftest tests
  (testing "Solves Part 1"
    (is (= (solve-part1 example) 514579))
    (is (= (solve-part1 input) 927684)))
  (testing "Solves Part 2"
    (is (= (solve-part2 example) 241861950))
    (is (= (solve-part2 input) 292093004))))


; Day 03 https://adventofcode.com/2020/day/3 in Clojure

(def day03-example "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn parse-tree-input [input]
  (mapv (fn [row]
          (mapv {\# 1 \. 0} row))
        (str/split-lines input)))

(defn tree-count
  [right down]
  (comp
    (map-indexed (fn [i x] (nth (cycle x) (* i right))))
    (drop (dec down))))

;; Day 3 part 1
(transduce (tree-count 3 1) + (parse-tree-input day03-example))

;; Day 3 part 2
(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(apply
  *
  (map
    #(transduce (apply tree-count %) + (parse-tree-input day03-example))
    slopes))


(def passports "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")


(defn parse-entry
  [entry]
  (->> (re-seq #"(\w{3}):(\S+)" entry)
       (mapcat next)
       (apply hash-map)))

(defn parse-passports [input]
  (map parse-entry (str/split input #"\R\R")))

(defn valid? [passport]
  (= 7 (count (dissoc passport :cid))))

(defn valid2? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (boolean
    (and
      byr (<= 1920 (Integer/parseInt byr) 2002)
      iyr (<= 2010 (Integer/parseInt iyr) 2020)
      eyr (<= 2020 (Integer/parseInt eyr) 2030)
      hgt (let [[_ value unit] (re-find #"(\d+)(cm|in)" hgt)]
            (case unit
              "cm" (<= 150 (Integer/parseInt value) 193)
              "in" (<= 59 (Integer/parseInt value) 76)
              false))
      hcl (re-find #"^#[0-9a-f]{6}$" hcl)
      ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
      pid (re-find #"^\d{9}$" pid))))


(valid2? {:pid "087499704"
          :hgt "74in"
          :ecl "grn"
          :iyr "2012"
          :eyr "2030"
          :byr "1980"
          :hcl "#623a2f"})

(let [example (parse-passports passports)]
  (count (filter valid? example)))
