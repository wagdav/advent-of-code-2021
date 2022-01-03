(ns aoc2021.day22
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[instr coord-range] (str/split line #" ")]
    (into [(keyword instr)]
          (->> (re-seq #"-?\d+" coord-range)
               (map #(Integer/parseInt %))
               (partition 2)
               (map vec)))))

(defn parse-input [input]
  (mapv parse-line (str/split-lines input)))

(defn initialize [instructions]
  (reduce
    (fn [state [cmd [x1 x2] [y1 y2] [z1 z2]]]
      (case cmd
        :on
        (reduce conj state (for [x (range (max -50 x1) (inc (min 50 x2)))
                                 y (range (max -50 y1) (inc (min 50 y2)))
                                 z (range (max -50 z1) (inc (min 50 z2)))] [x y z]))

        :off
        (reduce disj state (for [x (range (max -50 x1) (inc (min 50 x2)))
                                 y (range (max -50 y1) (inc (min 50 y2)))
                                 z (range (max -50 z1) (inc (min 50 z2)))] [x y z]))))
    #{}
    instructions))

(defn intersect [[[cx1 cx2] [cy1 cy2] [cz1 cz2] _]
                 [[dx1 dx2] [dy1 dy2] [dz1 dz2] s2]]
  (let [c (vector
            [(max cx1 dx1) (min cx2 dx2)]
            [(max cy1 dy1) (min cy2 dy2)]
            [(max cz1 dz1) (min cz2 dz2)]
            (- 0 s2))]
    (when (every? #(apply <= %) (butlast c))
      c)))

(defn cuboid-volume [[[x1 x2] [y1 y2] [z1 z2] sign]]
  (* sign (- (inc x2) x1) (- (inc y2) y1) (- (inc z2) z1)))

(defn total-volume [cuboids]
  (apply + (map cuboid-volume cuboids)))

; https://www.reddit.com/r/adventofcode/comments/rlxhmg/comment/hpoj9c6/
(defn reboot [instructions]
   (reduce
     (fn [cuboids [cmd xr yr zr]]
       (let [sign ({:on 1 :off -1} cmd)
             cur (vector xr yr zr sign)
             intersections (keep #(intersect cur %) cuboids)]
         (cond-> cuboids
           true
           (into intersections)

           (= cmd :on)
           (conj cur))))
     []
     instructions))

(defn solve-part1 [input]
  (count (initialize input)))

(defn solve-part2 [input]
  (total-volume (reboot input)))
