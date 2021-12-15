(ns aoc2021.day15
  (:require [clojure.string :as str]))

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
      (< 0    x) (conj [(dec x)      y])
      (< x imax) (conj [(inc x)      y])
      (< 0    y) (conj [     x  (dec y)])
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

(defn distance [p1 p2]
  (let [dx (- (first p1) (first p2))
        dy (- (second p1) (second p2))]
   (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn farther [p ps limit]
  (every? #(< limit (distance p %)) ps))

(defn least-risky-path [{:keys [super-size] :as grid}]
  (let [start [0 0]
        end [(dec super-size) (dec super-size)]]
    (loop [current start
           distances {current 0}
           visited #{}
           i 1]
      (if-let [min-risk (visited end)]
        (distances min-risk)
        (let [tentative-distances (->> (super-neigbours grid current)
                                       (map (fn [n]
                                                [n (+ (distances current)
                                                      (super-get grid n))]))
                                       (into {}))
              new-distances (reduce
                              (fn [result [k v]]
                                (assoc result k (min v (get result k v))))
                             distances
                             tentative-distances)
              next-point (first (apply min-key second (remove (fn [[k _]] (visited k)) new-distances)))]
          (recur next-point
                 new-distances
                 (conj visited current)
                 (inc i)))))))

(comment
  (def example (parse-input example-input))

  (-> (super-grid example 2)
      (super-neigbours [9 9]))

  (-> (super-grid example 5)
      (super-get [0 0]))

  (least-risky-path (super-grid example 1))
  (least-risky-path (super-grid example 5)))

(defn solve-part1 [input]
  (least-risky-path (super-grid input)))

(defn solve-part2 [input]
  (least-risky-path (super-grid input 5)))
