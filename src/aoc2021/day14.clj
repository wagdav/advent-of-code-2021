(ns aoc2021.day14
  (:require [clojure.string :as str]))

(defn parse-input [input]
  (let [words (re-seq #"\w+" input)]
    {:template (first words)
     :rules (apply hash-map (rest words))}))

(defn polymer-naive [rules template]
  (iterate
    (fn [t]
      (str
        (->> (partition 2 1 t)
             (map str/join)
             (map rules)
             (interleave t)
             (apply str))
        (last t)))
    template))

(def add (fnil + 0))
(def sub (fnil - 0))

(defn produce-pairs [pair rules]
  (let [middle (first (rules (str/join pair)))]
    [(list (first pair) middle)
     (list middle (second pair))]))

(defn polymer-fast [rules template]
  (iterate
    (fn [freqs]
      (reduce
        (fn [result [old amount new1 new2]]
          (-> result
              ; Transform `amount` number of `old` pairs into `new1` and `new2`.
              (update old sub amount)
              (update new1 add amount)
              (update new2 add amount)))
        freqs
        (map (fn [[pair amount]] (into [pair amount] (produce-pairs pair rules))) freqs)))
    (frequencies (partition 2 1 template))))

(defn pair-frequencies [initial m]
  (-> (reduce
        (fn [result [[p1 p2] v]]
            (-> result
                (update p1 add v)
                (update p2 add v)))
        {}
        m)
      ; Add back first and the last character of the initial template
      (update (first initial) add 1)
      (update (last initial) add 1)
      ; We counted everything twice, so divide the values by 2
      (#(zipmap (keys %)
                (map (fn [v] (quot v 2)) (vals %))))))

(defn solve-part1 [{:keys [template rules]}]
  (->> (nth (polymer-naive rules template) 10)
       frequencies
       vals
       (apply (juxt max min))
       (apply -)))

(defn solve-part2 [{:keys [template rules]}]
  (->> (nth (polymer-fast rules template) 40)
       (pair-frequencies template)
       vals
       (apply (juxt max min))
       (apply -)))
