(ns aoc2021.day21)

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (map #(Integer/parseInt %))
       (partition 2)
       (map second)))

(defn deterministic-die []
  (cycle (range 1 101)))

(defn move [pos step]
  (-> (+ pos step)
      dec
      (mod 10)
      inc))

(defn solve-part1 [input]
  (reduce
    (fn [[p1 p2 s1 s2 d] rolls]
      (let [new-p1 (reduce move p1 (take 3 rolls))
            new-p2 (reduce move p2 (drop 3 rolls))
            new-s1 (+ s1 new-p1)
            new-s2 (+ s2 new-p2)]
        (if (or (>= new-s1 1000) (>= new-s2 1000))
          (reduced
            (if (> new-s1 new-s2)
              (* s2 (+ 3 d))   ; Player 1 wins
              (* s1 (+ 3 d)))) ; Player 2 wins
          [new-p1 new-p2 (+ s1 new-p1) (+ s2 new-p2) (+ 6 d)])))
    [(first input) (second input) 0 0 0]
    (partition 6 (deterministic-die))))

(def dirac-roll
  (into [] (for [a [1 2 3] b [1 2 3] c [1 2 3]] [a b c])))

(defn next-turn [t]
  (-> t inc (mod 2)))

(def count-games
  (memoize
    (fn [positions scores turn]
      (loop [wins [0 0]
             rolls dirac-roll]
        (if (empty? rolls)
          wins
          (let [p (reduce move (positions turn) (peek rolls))
                s (+ (scores turn) p)]
            (if (>= s 21)
              (recur (update wins turn inc) (pop rolls))
              (let [wins' (count-games (assoc positions turn p)
                                       (assoc scores turn s)
                                       (next-turn turn))]
                (recur (mapv + wins' wins) (pop rolls))))))))))

(defn solve-part2 [input]
  (apply max (count-games (into [] input) [0 0] 0)))
