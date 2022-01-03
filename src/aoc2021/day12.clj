(ns aoc2021.day12
  (:require [clojure.string :as str]))

(def example-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn parse-input [input]
  (let [pairs (map #(str/split % #"-") (str/split-lines input))]
    (reduce
      (fn [graph [a b]]
        (-> graph
            (update a conj b)
            (update b conj a)))
      {}
      pairs)))

(defn big-cave? [s] (= s (str/upper-case s)))
(defn small-cave? [s] (and (= s (str/lower-case s))
                           (not= s "start")
                           (not= s "end")))

(defn candidates
  "Given a cave map and the path return the path candidates to continue"
  [cave-map path]
  (set (map #(conj path %) (cave-map (peek path)))))

(defn paths [pred cave-map]
  (loop [to-check (candidates cave-map ["start"])
         result #{}]
    (if (empty? to-check)
      result
      (let [current   (first to-check)
            remaining (rest to-check)]
        (if (= (peek current) "end")
          (recur remaining
                 (conj result current))
          (recur (into remaining (filter pred (candidates cave-map current)))
                 result))))))

(defn start-end-once? [path]
  (let [f (frequencies path)]
     (and (<= (get f "start" 0) 1)
          (<= (get f   "end" 0) 1))))

; Visit small caves at most once, and can visit big caves any number of times.
(defn allowed? [path]
  (and (start-end-once? path)
       (->> path
            (filter small-cave?)
            (frequencies)
            (every? (fn [[_ v]] (= v 1))))))

; Big caves can be visited any number of times, a single small cave can be
; visited at most twice.
(defn allowed2? [path]
  (and (start-end-once? path)
       (let [small-caves (filter small-cave? path)]
         (<= (- (count small-caves)
                (count (distinct small-caves)))
             1))))

(defn solve-part1 [input]
  (->> input (paths allowed?) count))

(defn solve-part2 [input]
  (->> input (paths allowed2?) count))
