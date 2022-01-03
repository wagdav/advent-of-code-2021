(ns aoc2021.day17)

(defn parse-input [input]
  (map
    #(Integer/parseInt %)
    (re-seq #"-?\d+" input)))

(def example-input "target area: x=20..30, y=-10..-5")

(defn drag [speed]
  (condp apply [0 speed]
    < (dec speed)
    = speed
    > (inc speed)))

(defn max-height [{:keys [y] :as state} max-y]
  (assoc state :max-y (max y max-y)))

(defn step [{:keys [t x y vx vy max-y]
             :or {t 0 x 0 y 0 vx 0 vy 0 max-y 0}
             :as state}]
  (-> state
    (assoc
       :t  (inc t)
       :x  (+ x vx)
       :y  (+ y vy)
       :vx (drag vx)
       :vy (dec vy))
    (max-height max-y)))

(defn in-target? [{:keys [x y]} [left right bot top]]
  (and (<= left x right) (<= bot y top)))

(defn can-reach-target? [{:keys [x y]} [_ right bot _]]
  (and (<= x right) (<= bot y)))

(defn shoot [vx vy target]
  (loop [state (step {:t 0 :x 0 :y 0 :vx vx :vy vy})]
    (if (can-reach-target? state target)
      (if (in-target? state target)
        state
        (recur (step state)))
      state)))

(defn scan-vy [[left right bot top]]
  (filter
    #(in-target? % [left right bot top])
    (for [vx (range 0 (inc right))
          vy (range bot (- 1 bot))]
      (shoot vx vy [left right bot top]))))

(defn solve-part1 [target]
  (->> (scan-vy target)
       (map :max-y)
       (apply max)))

(defn solve-part2 [target]
  (count (scan-vy target)))
