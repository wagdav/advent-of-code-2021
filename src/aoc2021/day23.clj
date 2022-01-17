(ns aoc2021.day23
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def example-input "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(defn parse-input [input]
  (->>
    (re-seq #"[A-D]" input)
    (map first)
    (partition 4)
    (apply mapv vector)
    (flatten)
    (map-indexed (fn [i a] [:room [(-> i (quot 2) inc (* 2)) (-> i (mod 2) inc)] a]))))

(def step-energy {\A 1 \B 10 \C 100 \D 1000})
(def dst-room {\A 2 \B 4 \C 6 \D 8})

(defn room-capacity [input]
  (case (count input)
    8  2
    16 4))

(defn goal [input]
  (map (fn [[pos [room-number room-position] _]]
         (vector pos [room-number room-position] ({2 \A 4 \B 6 \C 8 \D} room-number)))
       input))

; Between the first and second lines of text that contain amphipod starting
; positions, insert the following lines:

;  #D#C#B#A#
;  #D#B#A#C#
(def extra-input [[:room [2 3] \D]
                  [:room [2 4] \D]
                  [:room [4 3] \C]
                  [:room [4 4] \B]
                  [:room [6 3] \B]
                  [:room [6 4] \A]
                  [:room [8 3] \A]
                  [:room [8 4] \C]])

; For Part 2 insert the "forgotten" elements into the starting state
(defn unfold [input]
  (let [original (partition 2 input)
        extra (partition 2 extra-input)]
    (mapcat concat original extra)))

(defn locations [state]
  (reduce
    (fn [res [pos coord amphipod]]
      (case pos
        :room
        (assoc-in res (into [:rooms] coord) amphipod)

        :hallway
        (assoc-in res [:hallway coord] amphipod)))
    {}
    state))

(defn to-hallway-right [number hallway]
  (loop [dst (inc number) steps #{}]
    (if (or (> dst 10) (contains? hallway dst))
      steps
      (recur (inc dst) (conj steps [:hallway dst])))))

(defn to-hallway-left [number hallway]
  (loop [dst (dec number) steps #{}]
    (if (or (neg? dst) (contains? hallway dst))
      steps
      (recur (dec dst) (conj steps [:hallway dst])))))

(defn to-hallway [number hallway]
  (concat
    (to-hallway-left number hallway)
    (to-hallway-right number hallway)))

(defn room-to-hallway [number rooms hallway amphipod]
  (->>
    (to-hallway number hallway)
    (remove #(contains? rooms (second %))) ; no stopping in front of the rooms
    (map #(conj % amphipod))
    (into #{})))

(defn clean-room?
  "Empty room or only occupied by `a`-class  amphipods"
  [rooms a capacity]
  (let [dst (dst-room a)
        occupants (vals (get rooms dst {}))]
    (if (< (count occupants) capacity)
      (or (empty? occupants) (empty? (remove #{a} occupants)))
      false)))

(defn blocked-in-room? [[number position] rooms]
  (if (= position 1)
    false
    (contains? (rooms number) (dec position))))

(defn next-place [rooms amphipod capacity]
  (let [dst (dst-room amphipod)
        cnt (count (get rooms dst {}))]
    [dst (- capacity cnt)]))

(defn in-dst-room? [[number position] rooms amphipod capacity]
  (if (= number (dst-room amphipod))
    (every? #(= amphipod (get-in rooms [number %]))
            (range position (inc capacity)))
    false))

(defn to-destination [number rooms hallway amphipod capacity]
  (if (clean-room? rooms amphipod capacity)
    (->> (to-hallway number hallway)
         (filter #(= (dst-room amphipod) (second %)))
         (map (constantly (vector :room (next-place rooms amphipod capacity) amphipod))))
    []))

(defn from-room [[number position] rooms hallway amphipod capacity]
  (if (or (in-dst-room? [number position] rooms amphipod capacity)
          (blocked-in-room? [number position] rooms))
    #{}
    (set
      (concat
        (room-to-hallway number rooms hallway amphipod)
        (to-destination number rooms hallway amphipod capacity)))))

(defn possible-steps [state]
  (let [{:keys [rooms hallway]} (locations state)
        capacity (room-capacity state)]
    (mapcat
      (fn [[pos coord amphipod]]
        (map #(conj [[pos coord amphipod]] %)
          (case pos
            :room
            (from-room coord rooms hallway amphipod capacity)

            :hallway
            (to-destination coord rooms hallway amphipod capacity))))
      state)))

(defn cost [[pos1 ^long coord1 amphipod] [pos2 ^long coord2 _]]
  (case [pos1 pos2]
    [:room :hallway]
    (let [[^long n1 p1] coord1]
      (* (step-energy amphipod) (+ p1 (Math/abs (- n1 coord2)))))

    [:hallway :room]
    (let [[^long n2 p2] coord2]
      (* (step-energy amphipod) (+ p2 (Math/abs (- n2 coord1)))))

    [:room :room]
    (let [[^long n1 p1] coord1
          [^long n2 p2] coord2]
      (* (step-energy amphipod) (+ p1 p2 (Math/abs (- n1 n2)))))))

(defn tentative-states [state]
  (map
    (fn ([[from to]] (vector
                       (set (replace {from to} state))
                       (cost from to))))
    (possible-steps state)))

(defn min-energy [start end]
  (loop [frontier (priority-map start 0)
         explored #{}]
    (let [[current cost] (peek frontier)]
      (if (= current end)
        cost
        (recur
          (reduce
             (fn [queue [s tentative-cost]]
               (assoc queue s (min tentative-cost
                                   (get queue s tentative-cost))))

             (pop frontier)

             (->> (tentative-states current)
                  (remove explored)
                  (map (fn [[s c]] [s (+ cost c)]))))

          (conj explored current))))))

(defn solve-part1 [input]
  (let [start (set input)
        end (set (goal input))]
    (min-energy start end)))

(defn solve-part2 [input]
  (let [start (set (unfold input))
        end (set (goal start))]
    (min-energy start end)))
