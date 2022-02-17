(ns aoc2021.day08b
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.set :as set]
            [aoc2021.day08 :refer [decode]]))

(defn overlay [a b c]
  (project [a b c]
    (== true
        (= (set/union (set a) (set b)) (set c)))))

(defn decode-logic [candidates]
  (let [length (group-by count candidates)]
    (first
      (run 1 [q]
        (fresh [zero one two three four five six seven eight nine]
          (== q {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9})

          (== [one]   (length 2))
          (== [four]  (length 4))
          (== [seven] (length 3))
          (== [eight] (length 7))

          (permuteo (length 5) [two three five])
          (permuteo (length 6) [zero six nine])

          (overlay one three three)
          (overlay one nine nine)
          (overlay one zero zero)
          (overlay one five nine)

          (permuteo candidates [zero one two three four five six seven eight nine]))))))

(comment
  (def candidates ["be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb"])
  (time (decode candidates)) ; takes less than 1 msec
  (time (decode-logic candidates)) ; takes ~6 seconds
  (= (decode-logic candidates) (decode candidates)))
