(ns aoc-2022.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn ->section-pairs
  [s]
  (->> (str/split-lines s)
       (map #(next (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" %)))
       (s/transform [s/ALL s/ALL] parse-long)
       (map (partial split-at 2))))

(defn section-pair-fully-contained?
  [[[a b] [c d]]]
  (or (and (<= a c) (>= b d)) (and (<= c a) (>= d b))))

(defn section-pair-overlap?
  [[[a b] [c d]]]
  (or (and (<= a c) (>= b c)) (and (<= c a) (>= d a))))

(defn answer
  [s]
  (let [section-pairs (->section-pairs s)]
    {:part-1 (count (filter section-pair-fully-contained? section-pairs))
     :part-2 (count (filter section-pair-overlap? section-pairs))}))

(let [test-data (slurp (io/resource "aoc-2022/04/test.dat"))
      input-data (slurp (io/resource "aoc-2022/04/input.dat"))]
  (assert (= {:part-1 2 :part-2 4} (answer test-data)))
  (assert (= {:part-1 503 :part-2 827} (answer input-data))))
