(ns aoc-2022.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL transform]]))
(defn parse-uint [s] (Integer/parseUnsignedInt s))
(defn part-1 [data] (first data))
(defn part-2 [data] (apply + (take 3 data)))
(defn process
  ([data] {:part-1 (process part-1 data), :part-2 (process part-2 data)})
  ([f data]
   (->> (string/split data #"\n\n")
        (map string/split-lines)
        (transform [ALL ALL] parse-uint)
        (map (partial apply +))
        (sort >)
        f)))
(let [test-data
        "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
      input-data (slurp (io/resource "aoc-2022/01/input.dat"))]
  (assert (= {:part-1 24000, :part-2 45000} (process test-data)))
  (assert (= {:part-1 70374, :part-2 204610} (process input-data))))
