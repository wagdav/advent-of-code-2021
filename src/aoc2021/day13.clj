(ns aoc2021.day13
  (:require [clojure.string :as str]))

(def example-input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn parse-input [input]
  (let [[dots folds] (str/split input #"\R\R")]
    {:dots (->> (re-seq #"\d+" dots)
                (map read-string)
                (partition 2)
                (set))
     :folds (->> (str/split-lines folds)
                 (map #(str/split % #"="))
                 (map (fn [[k v]] [({"fold along x" :along-x "fold along y" :along-y} k)
                                   (read-string v)])))}))

(defn fold [along p]
  (if (< along p)
    (- (* 2 along) p)
    p))

(defn fold-paper [dots folds]
  (reduce
    (fn [paper [dir along]]
      (into #{}
        (case dir
          :along-x (map (fn [[x y]] [(fold along x) y]) paper)
          :along-y (map (fn [[x y]] [x (fold along y)]) paper))))
    dots
    folds))

(defn show-code [dots]
  (let [max-x (apply max (map first dots))
        max-y (apply max (map second dots))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (if (dots [x y])
          (print "#")
          (print " ")))
      (println))))

(defn solve-part1 [{:keys [dots folds]}]
  (count (fold-paper dots [(first folds)])))

(defn solve-part2 [{:keys [dots folds]}]
  (show-code (fold-paper dots folds)))
