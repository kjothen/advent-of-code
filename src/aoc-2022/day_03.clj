(ns aoc-2022.day-03
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn ->rucksacks [s] (str/split-lines s))

(defn priority
  [c]
  (if (Character/isLowerCase c)
    (+ 1 (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))

(defn common-compartment-items
  [rucksacks]
  (->> rucksacks
       (map (fn [items] (split-at (/ (count items) 2) items)))
       (s/transform [s/ALL s/ALL] set)
       (map (fn [compartment-items]
              (apply set/intersection compartment-items)))))

(defn common-group-items
  [rucksacks]
  (->> rucksacks
       (partition 3)
       (s/transform [s/ALL s/ALL] set)
       (map (fn [rucksack-items] (apply set/intersection rucksack-items)))))

(defn total-priorities
  [items]
  (->> (s/transform [s/ALL s/ALL] priority items)
       (map (partial apply +))
       (apply +)))

(defn answer
  [s]
  (let [rucksacks (->rucksacks s)
        compartment-items (common-compartment-items rucksacks)
        group-items (common-group-items rucksacks)]
    {:part-1 (->> compartment-items
                  total-priorities)
     :part-2 (->> group-items
                  total-priorities)}))

(let [test-data (slurp (io/resource "aoc-2022/03/test.dat"))
      input-data (slurp (io/resource "aoc-2022/03/input.dat"))]
  (assert (= {:part-1 157 :part-2 70} (answer test-data)))
  (assert (= {:part-1 7878 :part-2 2760} (answer input-data))))
