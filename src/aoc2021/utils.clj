(ns aoc2021.utils)

(defn irange
  "Like range, but inclusive and it's smarter about upper and lower limits"
   [a b]
   (if (<= a b)
     (range a (inc b) 1)
     (range a (dec b) -1)))

(comment
  (irange 1 5)   ; (1 2 3 4 5)
  (irange 10 1)) ; (5 4 3 2 1)
