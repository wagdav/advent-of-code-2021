(ns aoc2021.day16
  (:require [clojure.string :as str]))

(def hex-digits { \0 [0 0 0 0] \1 [0 0 0 1] \2 [0 0 1 0] \3 [0 0 1 1]
                  \4 [0 1 0 0] \5 [0 1 0 1] \6 [0 1 1 0] \7 [0 1 1 1]
                  \8 [1 0 0 0] \9 [1 0 0 1] \A [1 0 1 0] \B [1 0 1 1]
                  \C [1 1 0 0] \D [1 1 0 1] \E [1 1 1 0] \F [1 1 1 1]})

(def packet-type {0 :sum
                  1 :prod
                  2 :min
                  3 :max
                  4 :literal
                  5 :gt
                  6 :lt
                  7 :eq})

(defn to-decimal [binary-seq]
  (Long/parseLong (str/join binary-seq) 2))

(defn version [{:keys [input] :as state}]
  (assoc state
    :version (to-decimal (take 3 input))
    :input (drop 3 input)))

(defn id [{:keys [input] :as state}]
  (assoc state :id (to-decimal (take 3 input))
               :input (drop 3 input)))

(defn literal [{:keys [input id] :as state}]
  {:pre [(= id 4)]}
  (merge state
    (loop [in input
           result []
           processed 0]
      (if (zero? (first in))
        (let [value (into result (take 4 (rest in)))
              packet-size (* 5 (inc processed))]
          {:value (to-decimal value)
           :input (drop (+ packet-size) input)})
        (recur
          (drop 5 in)
          (into result (take 4 (rest in)))
          (inc processed))))))

(declare parse-binary-packet)

(defn operator [{:keys [id input] :as state}]
  {:pre
   [(not= id 4) (packet-type id)]}

  (let [[length-type & packet] input]
    (case length-type
      0 (let [[length remaining]           (split-at 15 packet)
              [sub-packet remaining-input] (split-at (to-decimal length) remaining)]
          (merge state
            {:operands
               (loop [stream sub-packet
                      result []]
                (if (empty? stream)
                  result
                  (let [next-packet (parse-binary-packet stream)]
                    (recur
                      (:input next-packet)
                      (conj
                        result
                        (dissoc next-packet :input))))))
             :input remaining-input
             :length-type 0}))

      1 (let [[num-subpackets remaining] (split-at 11 packet)]
          (merge state
            (loop [stream remaining
                   result []
                   n (to-decimal num-subpackets)]
              (if (zero? n)
                {:operands result
                 :input stream
                 :length-type 1}

                (let [next-packet (parse-binary-packet stream)]
                  (recur
                    (:input next-packet)
                    (conj
                      result
                      (dissoc next-packet :input))
                    (dec n))))))))))

(defn parse-binary-packet [binary]
  (let [p (-> {:input binary} version id)
        id (p :id)]
    (assoc
      (if (= id 4)
        (literal p)
        (operator p))
      :type (packet-type id))))

(defn parse-input [input]
     (->> input
          (replace hex-digits)
          (apply concat)
          (parse-binary-packet)))

(defn total-version [packet]
  (if (packet :value)
    (:version packet)
    (+
     (:version packet)
     (->> (:operands packet)
          (map total-version)
          (apply +)))))

(defn eval-packet [packet]
  (let [with-operands (fn [op] (->> (:operands packet)
                                    (map eval-packet)
                                    (apply op)))]
    (case (packet :type)
      :literal (packet :value)
      :sum     (with-operands +)
      :prod    (with-operands *)
      :min     (with-operands min)
      :max     (with-operands max)
      :gt      (if (with-operands >) 1 0)
      :lt      (if (with-operands <) 1 0)
      :eq      (if (with-operands =) 1 0))))

(defn solve-part1 [input]
  (total-version (parse-input input)))

(defn solve-part2 [input]
  (eval-packet (parse-input input)))
