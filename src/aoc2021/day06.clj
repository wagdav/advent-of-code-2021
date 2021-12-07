(ns aoc2021.day06)

(def example-input "3,4,3,1,2")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map read-string)))

(defn solve-part1 [input]
   (loop [f input
          t 80]
     (if (= t 0)
       (count f)
       (recur (mapcat (fn [f] (if (= 0 f) [6 8] [(dec f)])) f)
              (dec t)))))

(defn fish [n input]
   (loop [fish (frequencies input)
          t    n]
     (if (= t 0)
        (apply + (vals fish))
        (recur (-> (into {} (for [i (range 9)]
                                 [i
                                  (get fish (mod (inc i) 9) 0)]))
                   (update 6 + (get fish 0 0)))
               (dec t)))))

(defn solve-part2 [input]
  (fish 256 input))

(comment
  (def input (frequencies (parse-input example-input)))

  ; The laternfish evolution rule as a lazy sequence.
  (defn fish-lazy-seq [st]
    (let [new-st (-> (into {} (for [i (range 9)] [i (get st (mod (inc i) 9) 0)]))
                     (update 6 + (get st 0 0)))]
      (lazy-seq (cons st (fish-lazy-seq new-st)))))

  ; Same as `fish-lazy-seq`, using `iterate`.  Seems clearer.
  (defn fish-iterate [state]
    (iterate
      (fn [st]
       (-> (into {} (for [i (range 9)] [i (get st (mod (inc i) 9) 0)]))
           (update 6 + (get st 0 0))))
      state))

  ; The final aggregation function.
  (defn fish-count [st] (apply + (vals st)))

  ; This decoupling allows to ask different questions.  For example, see the
  ; first few days:
  (take 8 (fish-iterate input))

  ; At which day do we overcome 10k lanternfish?
  (->> (fish-iterate input)
       (map-indexed (fn [idx st] [idx (fish-count st)]))
       (drop-while (fn [[_ cnt]] (<= cnt 10000)))
       ffirst)

  ; Solution of the puzzle: "How many lanterfish would there be after 256
  ; days?"
  (-> (nth (fish-iterate input) 256)
      fish-count))
