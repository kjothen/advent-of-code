(ns aoc-2022.day-03
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]
            [com.rpl.specter :refer [ALL transform]]))

(defn priority
  [c]
  (if (Character/isLowerCase c)
    (+ 1 (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))

(defn common-compartment-items
  [rucksacks]
  (->> rucksacks
       (map (fn [items] (split-at (/ (count items) 2) items)))
       (transform [ALL ALL] set)
       (map (fn [compartment-items]
              (apply clojure.set/intersection compartment-items)))))

(defn common-group-items
  [rucksacks]
  (->> rucksacks
       (partition 3)
       (transform [ALL ALL] set)
       (map (fn [rucksack-items]
              (apply clojure.set/intersection rucksack-items)))))

(defn items->priorities [items] (transform [ALL ALL] priority items))

(defn total-priorities
  [priorities]
  (->> priorities
       (map (partial apply +))
       (apply +)))

(defn answer
  [s]
  (let [rucksacks (str/split-lines s)
        compartment-items (common-compartment-items rucksacks)
        group-items (common-group-items rucksacks)]
    {:part-1 (->> compartment-items
                  items->priorities
                  total-priorities)
     :part-2 (->> group-items
                  items->priorities
                  total-priorities)}))

(let [test-data (slurp (io/resource "aoc-2022/03/test.dat"))
      input-data (slurp (io/resource "aoc-2022/03/input.dat"))]
  (assert (= {:part-1 157 :part-2 70} (answer test-data)))
  (assert (= {:part-1 7878 :part-2 2760} (answer input-data))))
