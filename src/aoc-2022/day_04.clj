(ns aoc-2022.day-04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL transform]]))
(defn parse-uint [s] (Integer/parseUnsignedInt s))
(defn filter-1
  [[x y x' y']]
  (or (and (<= x x') (>= y y')) (and (<= x' x) (>= y' y))))
(defn filter-2
  [[x y x' y']]
  (or (and (<= x x') (>= y x')) (and (<= x' x) (>= y' x))))
(defn process
  ([data] {:part-1 (process filter-1 data), :part-2 (process filter-2 data)})
  ([filter-fn data]
   (->> (string/split-lines data)
        (map #(next (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" %)))
        (transform [ALL ALL] parse-uint)
        (filter filter-fn)
        count)))
(let [test-data "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
      input-data (slurp (io/resource "aoc-2022/04/input.dat"))]
  (assert (= {:part-1 2, :part-2 4} (process test-data)))
  (assert (= {:part-1 503, :part-2 807})))
