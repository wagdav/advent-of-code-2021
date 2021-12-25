(ns aoc2021.day24
  (:require [clojure.string :as str]))

(def example-input "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2")

(defn parse-number [n]
  (try
    (Integer/parseInt n)
    (catch Exception _
      n)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(str/split % #" "))
       (mapv (fn [[instr dst src]] (vector (keyword instr) (keyword dst) (parse-number src))))))

(defn lookup [state value]
  (if (number? value)
    value
    (state (keyword value))))

(defn exec [{:keys [input] :as state} [instr dst src]]
  (case instr
    :inp
    (assoc state dst (first input) :input (rest input))

    :add
    (update state dst + (lookup state src))

    :mul
    (update state dst * (lookup state src))

    :div
    (update state dst quot (lookup state src))

    :mod
    (update state dst mod (lookup state src))

    :eql
    (assoc state dst (if (= dst (lookup state src)) 1 0))))

(defn run-program [prog input]
  (reduce
    exec
    {:w 0 :x 0 :y 0 :z 0 :input input}
    prog))

(defn parse-parameters [input]
  (->> input
       (partition 18)
       (map-indexed (fn [i row] {:div (last (nth row 4))
                                 :chk (last (nth row 5))
                                 :add (last (nth row 15))
                                 :i i}))))

; https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpuxf5l/
(defn run-program-smart [program ws]
  (let [params (parse-parameters program)]
    (reduce
      (fn [{:keys [stack inp] :as state} {:keys [i div chk add]}]
        (case div
          1
          (assoc state :stack (conj stack [i add]))

          26
          (let [[j add] (peek stack)
                inp-i  (+ (inp j) add chk)]
            (assoc state
              :stack (pop stack)
              :inp (cond-> inp
                      true
                      (assoc i inp-i)

                      (> inp-i 9)
                      (assoc j (- (inp j) (- inp-i 9))
                             i 9)

                      (< inp-i 1)
                      (assoc j (+ (inp j) (- 1 inp-i))
                             i 1))))))

      {:stack [] :inp (into [] ws)}
      params)))

(defn solve-part1 [input]
  (->> (run-program-smart input (repeat 14 9))
       :inp
       (apply str)
       Long/parseLong))

(defn solve-part2 [input]
  (->> (run-program-smart input (repeat 14 1))
       :inp
       (apply str)
       Long/parseLong))
