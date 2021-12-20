(ns aoc2021.day20
  (:require [clojure.string :as str]
            [aoc2021.utils :refer [irange]]))

(defn parse-image [text]
  (let [rows (str/split-lines text)
        size (count rows)]
    (into {} (for [x (range size)
                   y (range size)]
               [[x y] (get-in rows [x y])]))))

(defn parse-input [input]
  (let [[algorithm image] (str/split input #"\R\R")]
    {:algorithm algorithm
     :image (parse-image image)
     :infinity \.}))

(defn near [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1]] [(+ x dx) (+ y dy)]))

(defn index [image p infinity]
  (->> (near p)
       (map #(get image % infinity))
       (map {\# 1 \. 0})
       (apply str)
       (#(Integer/parseInt % 2))))

(defn coord-limits [image]
  (let [min-x (first (apply min-key first (keys image)))
        min-y (second (apply min-key second (keys image)))
        max-x (first (apply max-key first (keys image)))
        max-y (second (apply max-key second (keys image)))]
    [(min min-x min-y) (max max-x max-y)]))

(defn enhance [{:keys [image algorithm infinity] :as input}]
  (let [[min-coord max-coord] (coord-limits image)
        new-infinity (if (= \. infinity) (first algorithm) (last algorithm))]

    (assoc input
      :infinity new-infinity
      :image
       (reduce
         (fn [state p]
           (assoc state p (get algorithm (index image p infinity))))
         {}
         (for [x (irange (dec min-coord) (inc max-coord))
               y (irange (dec min-coord) (inc max-coord))] [x y])))))

(defn show [{:keys [image]}]
  (let [[min-coord max-coord] (coord-limits image)]
    (doseq [x (irange min-coord max-coord)]
     (doseq [y (irange min-coord max-coord)]
           (if-let [v (image [x y])]
             (print v)
             (print ".")))
     (println))))

(defn solve-part1 [input]
  (->> input
       enhance
       enhance
       :image
       vals
       (filter #{\#})
       count))

(defn solve-part2 [input]
  (->> (nth (iterate enhance input) 50)
       :image
       vals
       (filter #{\#})
       count))
