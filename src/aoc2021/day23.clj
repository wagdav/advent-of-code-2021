(ns aoc2021.day23
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def example-input "#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########")

(defn pad [n coll v]
  (take n (concat coll (repeat v))))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        cols (count (first lines))]
    (->>
      lines
      (mapv #(replace {\space \#} %))
      (mapv #(pad cols % \#))
      (map #(apply vector %)))))

(def step-energy {\A 1 \B 10 \C 100 \D 1000})
(def dst-room {\A 2 \B 4 \C 6 \D 8})

(parse-input example-input)

;#..2.4.6.8..#
;###B#C#B#D###
;  #A#D#C#A#
(def example-state
  #{[:room [2 1] \B]
    [:room [2 2] \A]
    [:room [4 1] \C]
    [:room [4 2] \D]
    [:room [6 1] \B]
    [:room [6 2] \C]
    [:room [8 1] \D]
    [:room [8 2] \A]})

; Between the first and second lines of text that contain amphipod starting
; positions, insert the following lines:

;  #D#C#B#A#
;  #D#B#A#C#

;###B#C#B#D###
;  #D#C#B#A#
;  #D#B#A#C#
;  #A#D#C#A#
(def example-state-part2
  #{[:room [2 1] \B]
    [:room [2 2] \D]
    [:room [2 3] \D]
    [:room [2 4] \A]

    [:room [4 1] \C]
    [:room [4 2] \C]
    [:room [4 3] \B]
    [:room [4 4] \D]

    [:room [6 1] \B]
    [:room [6 2] \B]
    [:room [6 3] \A]
    [:room [6 4] \C]

    [:room [8 1] \D]
    [:room [8 2] \A]
    [:room [8 3] \C]
    [:room [8 4] \A]})

(def real-state
  #{[:room [2 1] \C]
    [:room [2 2] \D]
    [:room [4 1] \A]
    [:room [4 2] \C]
    [:room [6 1] \B]
    [:room [6 2] \A]
    [:room [8 1] \D]
    [:room [8 2] \B]})

(def real-state-part2
  #{[:room [2 1] \C]
    [:room [2 2] \D]
    [:room [2 3] \D]
    [:room [2 4] \D]

    [:room [4 1] \A]
    [:room [4 2] \C]
    [:room [4 3] \B]
    [:room [4 4] \C]

    [:room [6 1] \B]
    [:room [6 2] \B]
    [:room [6 3] \A]
    [:room [6 4] \A]

    [:room [8 1] \D]
    [:room [8 2] \A]
    [:room [8 3] \C]
    [:room [8 4] \B]})

(def goal
  #{[:room [2 1] \A]
    [:room [2 2] \A]
    [:room [4 1] \B]
    [:room [4 2] \B]
    [:room [6 1] \C]
    [:room [6 2] \C]
    [:room [8 1] \D]
    [:room [8 2] \D]})

(def goal-part2
  #{[:room [2 1] \A]
    [:room [2 2] \A]
    [:room [2 3] \A]
    [:room [2 4] \A]
    [:room [4 1] \B]
    [:room [4 2] \B]
    [:room [4 3] \B]
    [:room [4 4] \B]
    [:room [6 1] \C]
    [:room [6 2] \C]
    [:room [6 3] \C]
    [:room [6 4] \C]
    [:room [8 1] \D]
    [:room [8 2] \D]
    [:room [8 3] \D]
    [:room [8 4] \D]})

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
    (if (or (< dst 0) (contains? hallway dst))
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
    (->>
      (concat
        (room-to-hallway number rooms hallway amphipod)
        (to-destination number rooms hallway amphipod capacity))
      (into #{}))))

(defn possible-steps [state]
  (let [{:keys [rooms hallway]} (locations state)
        capacity 2]
    (mapcat
      (fn [[pos coord amphipod]]
        (map #(conj [[pos coord amphipod]] %)
          (case pos
            :room
            (from-room coord rooms hallway amphipod capacity)

            :hallway
            (to-destination coord rooms hallway amphipod capacity))))
      state)))

(defn cost [[pos1 coord1 amphipod] [pos2 coord2 _]]
  (case [pos1 pos2]
    [:room :hallway]
    (let [[n1 p1] coord1]
      (* (step-energy amphipod) (+ p1 (Math/abs (- n1 coord2)))))

    [:hallway :room]
    (let [[n2 p2] coord2]
      (* (step-energy amphipod) (+ p2 (Math/abs (- n2 coord1)))))

    [:room :room]
    (let [[n1 p1] coord1
          [n2 p2] coord2]
      (* (step-energy amphipod) (+ p1 p2 (Math/abs (- n1 n2)))))))

(defn tentative-states [state]
  (->> (possible-steps state)
       (map (fn ([[from to]] (vector
                               (into #{} (replace {from to} state))
                               (cost from to)))))))

(defn min-energy [start end]
    (loop [frontier (priority-map start 0)
           explored #{}
           i 0]
      (let [[current cost] (peek frontier)]
        ;(println current cost)
        (if (or (> i 500000) (= current end))
          [cost i]
          (recur
            (reduce
               (fn [queue [s tentative-cost]]
                 (assoc queue s (min tentative-cost
                                     (get queue s tentative-cost))))

               (pop frontier)

               (->> (tentative-states current)
                    (map (fn [[s c]] [s (+ cost c)]))
                    (remove (fn [[s _]] (explored s)))))

            (conj explored current)

            (inc i))))))

(comment
  (possible-steps example-state)

  (blocked-in-room? [2 2] (:rooms (locations example-state)))
  (tentative-states example-state)

  (min-energy example-state goal)
  (time (min-energy real-state goal))

  ;Part2
  (min-energy example-state-part2 goal-part2)
  (time (min-energy real-state-part2 goal-part2)))

(defn solve-part1 [])
(defn solve-part2 [])
