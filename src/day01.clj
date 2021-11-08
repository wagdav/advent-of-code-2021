(ns aoc2021.day01
  (:require [clojure.test :as t :refer [deftest is testing run-tests]]))

(defn example []
  (println "Day 01")
  42)

(deftest test-example
  (is (= (example) 42)))

(run-tests)
