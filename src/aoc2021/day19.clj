(ns aoc2021.day19
  (:require [clojure.string :as str]))

(defn parse-block [block]
  (let [[header & body] (str/split-lines block)
        idx (->> header (re-find #"\d+") Integer/parseInt)
        coords (->> body
                    (map #(re-seq #"-?\d+" %))
                    (mapv #(mapv read-string %)))]
    {idx coords}))

(defn parse-input [input]
  (->> (str/split input #"\R\R")
       (map parse-block)
       (apply merge)))

(defn along+   [[x y z]] [x    y     z])
(defn along-   [[x y z]] [x    y  (- z)])
(defn up-x     [[x y z]] [y    z     x])
(defn up-y     [[x y z]] [z    x     y])
(defn up-z     [[x y z]] [x    y     z])
(defn rotate-z [[x y z]] [y (- x)    z])

(def variants-wrong
  (for [along  [along+ along-]
        rotate [along+ rotate-z (comp rotate-z rotate-z) (comp rotate-z rotate-z rotate-z)]
        up     [up-x up-y up-z]]
    (comp rotate along up)))

(def variants
  [
   (fn [[x y z]] [x, y, z])
   (fn [[x y z]] [z, y, (- x)])
   (fn [[x y z]] [(- x), y, (- z)])
   (fn [[x y z]] [(- z), y, x])
   (fn [[x y z]] [(- x), (- y), z])
   (fn [[x y z]] [(- z), (- y), (- x)])
   (fn [[x y z]] [x, (- y), (- z)])
   (fn [[x y z]] [z, (- y), x])
   (fn [[x y z]] [x, (- z), y])
   (fn [[x y z]] [y, (- z), (- x)])
   (fn [[x y z]] [(- x), (- z), (- y)])
   (fn [[x y z]] [(- y), (- z), x])
   (fn [[x y z]] [x, z, (- y)])
   (fn [[x y z]] [(- y), z, (- x)])
   (fn [[x y z]] [(- x), z, y])
   (fn [[x y z]] [y, z, x])
   (fn [[x y z]] [z, x, y])
   (fn [[x y z]] [y, x, (- z)])
   (fn [[x y z]] [(- z), x, (- y)])
   (fn [[x y z]] [(- y), x, z])
   (fn [[x y z]] [(- z), (- x), y])
   (fn [[x y z]] [y, (- x), z])
   (fn [[x y z]] [z, (- x), (- y)])
   (fn [[x y z]] [(- y), (- x), (- z)])])

(defn reading-variants [reading]
  (for [v variants]
    (map v reading)))

(defn translate [scan dv]
  (for [s scan]
    (mapv + s dv)))

(defn find-scanner [s variants]
  (some
    (fn [curr]
      (let [f (frequencies (for [x curr y s] (map - y x)))
            [diff cnt] (apply max-key val f)]
        (when (<= 12 cnt)
          [diff (translate curr diff)])))
    variants))

; https://github.com/taddeus/advent-of-code/blob/master/2021/19_beacon.py
(defn beacons [input]
  (loop [queue (into clojure.lang.PersistentQueue/EMPTY (map reading-variants (vals input)))
         result (set (input 0))
         scanners []]

    (if (empty? queue)
      {:beacons result :scanners scanners}

      (let [current (peek queue)
            remaining (pop queue)
            [scanner moved] (find-scanner result current)]
        (if scanner
          (recur remaining (into result moved) (conj scanners scanner))
          (recur (conj remaining current) result scanners))))))

(defn manhattan [p1 p2]
  (->> (map - p1 p2)
       (map #(Math/abs ^long %))
       (apply +)))

(defn solve-part1 [input]
  (count (:beacons (beacons input))))

(defn solve-part2 [input]
  (let [scanners (:scanners (beacons input))]
    (apply max (for [s1 scanners s2 scanners] (manhattan s1 s2)))))
