(ns aoc2021.day18
  (:require [clojure.string :as str]))

(def example-input "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(defn parse-input [input]
  (map read-string (str/split-lines input)))

(defn index-tree
  ([node]
   (index-tree node 0 -1))

  ([node level index]
   (if (:value node)
     (assoc node :index (inc index))

     (let [ltree (index-tree (:left node)  (inc level) index)
           rtree (index-tree (:right node) (inc level) (:index ltree))]
       (assoc node
              :left ltree
              :right rtree
              :level level
              :index (:index rtree))))))

(defn tree
  ([pair]
   (index-tree (tree pair 0)))

  ([pair _]
   (if (number? pair)
     {:value pair}
     {:left (tree (first pair) 0)
      :right (tree (second pair) 0)})))

(defn join [t1 t2]
  (index-tree {:left t1 :right t2}))

(defn split? [node]
  (if (:value node)
    (let [{:keys [value index]} node]
      ; If any regular number is 10 or greater, the leftmost such regular number splits.
      (when (<= 10 value)
        {:split index :value value}))

    (or
      (split? (:left node))
      (split? (:right node)))))

(defn explode? [node]
  (when (:level node)
    (or
      (let [{:keys [level index]} node]
        (when (= 4 level)
          {:explode index
           :left-index (get-in node [:left :index])
           :right-index (get-in node [:right :index])
           :left-value (get-in node [:left :value])
           :right-value (get-in node [:right :value])}))

      (explode? (:left node))
      (explode? (:right node)))))

(defn apply-action
  ([action node]
   (index-tree (apply-action action node 0)))

  ([action node _]
   (if (:value node)
    (let [{:keys [value index]} node]
      (cond
        (= (action :split) index)
        {:left {:value (int (Math/floor (/ value 2)))}
         :right {:value (int (Math/ceil (/ value 2)))}}

        (and (action :explode) (= index (dec (action :left-index)))) ; left neigbour of the exploding pair
        (update node :value + (action :left-value))

        (and (action :explode) (= index (inc (action :right-index)))) ; right neigbour of the exploding pair
        (update node :value + (action :right-value))

        :else
        node))

    (if (and (= (action :explode) (node :index)) (= (node :level) 4))
       {:value 0}

     (join (apply-action action (node :left) 0)
           (apply-action action (node :right) 0))))))

(defn magnitude [node]
  (if-let [mag (node :value)]
    mag
    (+ (* 3 (magnitude (node :left))) (* 2 (magnitude (node :right))))))

(defn to-vector [node]
  (if-let [mag (node :value)]
    mag
    [(to-vector (node :left)) (to-vector (node :right))]))

(defn add [f1 f2]
  (loop [result (join f1 f2)]
    (let [explode (explode? result)
          split   (split? result)]
      (cond
        explode
        (recur (apply-action explode result))

        split
        (recur (apply-action split result))

        :else
        result))))

(defn solve-part1 [input]
  (magnitude (reduce add (map tree input))))

(defn solve-part2 [input]
  (apply max
    (for [x (map tree input)
          y (map tree input)]
      (magnitude (add x y)))))
