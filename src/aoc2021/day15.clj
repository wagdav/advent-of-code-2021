(ns aoc2021.day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def example-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(re-seq #"\d" %))
       (mapv #(mapv read-string %))))

(defn neigbours [[x y] size]
  (let [imax (dec size)]
    (cond-> []
      (pos?   x) (conj [(dec x)      y])
      (< x imax) (conj [(inc x)      y])
      (pos?   y) (conj [     x  (dec y)])
      (< y imax) (conj [     x  (inc y)]))))

(defn super-grid
  ([grid]
   (super-grid grid 1))

  ([grid multiplier]
   {:grid grid
    :size (count grid)
    :super-size (* (count grid) multiplier)}))

(defn super-get [{:keys [grid size]} [x y]]
  (let [sx    (quot x size)
        sy    (quot y size)
        x     (rem x size)
        y     (rem y size)
        value (+ sx sy (get-in grid [x y]))]

    (inc (rem (dec value) 9))))

(defn super-neigbours [{:keys [super-size]} [x y]]
  (neigbours [x y] super-size))

(defn least-risky-path [{:keys [super-size] :as grid}]
  (let [start [0 0]
        end [(dec super-size) (dec super-size)]]
    (loop [frontier (priority-map start 0)
           explored #{}]
      (let [[current distance] (peek frontier)]
        (if (= current end)
          distance
          (recur
            (reduce
               (fn [queue [point tentative-distance]]
                 (assoc queue point (min tentative-distance
                                         (get queue point tentative-distance))))

               (pop frontier)

               (->> (super-neigbours grid current)
                    (remove explored)
                    (map #(vector % (+ distance (super-get grid %))))))

            (conj explored current)))))))

(defn solve-part1 [input]
  (least-risky-path (super-grid input)))

(defn solve-part2 [input]
  (least-risky-path (super-grid input 5)))
