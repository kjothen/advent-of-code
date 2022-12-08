(ns aoc-2022.day-03
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL transform]]))
(defn prioritize
  [c]
  (if (Character/isLowerCase c)
    (+ 1 (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))
(defn part-1
  [data]
  (->> data
       (map (fn [x] (split-at (/ (count x) 2) x)))
       (map (fn [[x y]] (clojure.set/intersection (set x) (set y))))))
(defn part-2
  [data]
  (->> data
       (partition 3)
       (map (fn [[x y z]] (clojure.set/intersection (set x) (set y) (set z))))))
(defn process
  ([data] {:part-1 (process part-1 data), :part-2 (process part-2 data)})
  ([f data]
   (->> (string/split-lines data)
        f
        (transform [ALL ALL] prioritize)
        (map (partial apply +))
        (apply +))))
(let
  [test-data
     "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
   input-data (slurp (io/resource "aoc-2022/03/input.dat"))]
  (assert (= {:part-1 157, :part-2 70} (process test-data)))
  (assert (= {:part-1 7878, :part-2 2760} (process input-data))))
