(ns aoc2021.day17)

(defn parse-input [input]
  (->> (re-seq #"-?\d+" input)
       (map #(Integer/parseInt %))))

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

(defn probe [vx vy]
  (iterate step {:t 0 :x 0 :y 0 :vx vx :vy vy}))

(step {:vx 6 :vy 7})

(take 8 (probe 7 2))

(take-while (fn [state] (<= (state :t) 10)) (probe 6 9))

(defn in-target? [{:keys [x y]} [left right bot top]]
  (and (<= left x right)
       (<= bot y top)))

(in-target? {:x 21 :y -6} [20 30 -10 -5])

(defn until-x [x-limit vx]
  (loop [state (step {:vx vx :vy 0})]
    (if (or (= 0 (state :vx)) (<= x-limit (state :x)))
      state
      (recur (step state)))))

(until-x 20 9)

(defn scan-vx [[left right _ _]]
  (loop [vx     1
         result []]

    (let [final (until-x right vx)]
      (println "vx=" vx "final=" final)
      (cond
        (< (final :x) left)
        (recur (+ vx 1) result)

        (<= left (final :x) right)
        (recur (+ vx 1) (conj result vx))

        :else
        result))))

(scan-vx target)

(defn until-t [vx vy [left right bottom top]]
  (loop [state (step {:t 0 :x 0 :y 0 :vx vx :vy vy})]
    (cond
      (in-target? state [left right bottom top])
      (assoc state :end :on-target)

      (<= (state :y) bottom)
      (assoc state :end :bottom)

      (<= right (state :x))
      (assoc state :end :right)

      :else
      (recur (step state)))))

(defn scan-vy [vxs [left right bot top]]
  (for
    [vx vxs]
    (loop [vy -100
           heights []
           dir :under]

      (let [final (until-t vx vy [left right bot top])]
        (do
          (println "vx=" vx "vy=" vy "final=" final)
          (cond
            (= (:end final) :on-target)
            (recur (inc vy) (conj heights (final :max-y)) :over)

            (and (= (:end final) :bottom)
                 (= dir :under))
            (recur (inc vy) heights :under)

            :else
            heights))))))

(defn solve-part1 [target]
  (apply max (flatten (scan-vy (scan-vx target) target))))

(defn solve-part2 [input])

(comment
  (def target [20 30 -10 -5])
  (scan-vy (scan-vx target) target)

  (def target [244 303 -91 -54])
  (scan-vy (scan-vx target) target)

  (solve-part1 target))

