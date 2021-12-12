(ns aoc2021.day11
  (:require [clojure.string :as str]))

(def example-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(re-seq #"\w" %))
       (mapv #(mapv read-string %))))

(defn neigbours [[px py] size]
  (let [max-index (dec size)]
    (for [x (range (dec px) (+ 2 px))
          y (range (dec py) (+ 2 py))
          :when (and (not= [x y] [px py])
                     (<= 0 x max-index)
                     (<= 0 y max-index))]
        [x y])))

(defn each-point-coords [grid]
  (let [size (count grid)]
    (for [x (range size) y (range size)] [x y])))

(defn increase-all-by-one [{:keys [grid] :as state}]
  (assoc state :grid
    (reduce
      (fn [grid p] (update-in grid p inc))
      grid
      (each-point-coords grid))))

(defn reset
  ; Any octopus that flashed during this step has its energy level set to 0, as
  ; it used all of its energy to flash.
  [{:keys [grid flashed] :as state}]
  (assoc state :grid
    (reduce
      (fn [state p] (assoc-in state p 0))
      grid
      flashed)))

(defn flash
  ([{:keys [grid]}]
   (loop [grid grid
          flashed #{}]
    (let [new-state (flash grid flashed)]
      (if (= (:flashed new-state) flashed)
          new-state  ; No more new flashes
          (recur (:grid new-state) (:flashed new-state))))))

  ([input flashed]
   (reduce
     (fn [{:keys [grid flashed]} p]
       {:grid (flash grid flashed p)
        :flashed (conj flashed p)})

     {:grid input :flashed flashed}

     (->> (each-point-coords input)
          (filter #(and (< 9 (get-in input %))
                        (nil? (flashed %)))))))

  ([input _ [x y]]
   (let [size (count input)]
      (reduce
        (fn [state p] (update-in state p inc))
        input
        (neigbours [x y] size)))))

(defn octopus [input]
  (iterate
    (fn [state]
      (->> state
           increase-all-by-one
           flash
           reset))
   {:grid input :flashed #{}}))

(defn solve-part1 [grid]
  (->> (take 101 (octopus grid))
       (map :flashed)
       (map count)
       (apply +)))

(defn solve-part2 [grid]
  (let [size (count grid)]
    (->> (map-indexed (fn ([i state] (assoc state :step i))) (octopus grid))
         (drop-while (fn [{:keys [flashed]}]
                       (not= (count flashed) (* size size))))
         first
         :step)))
