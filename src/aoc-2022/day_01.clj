(ns aoc-2022.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn ->elf-calories
  [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (s/transform [s/ALL s/ALL] parse-long)
       (map (partial apply +))))

(defn answer
  [s]
  (let [sorted-elf-calories (sort > (->elf-calories s))]
    {:part-1 (first sorted-elf-calories)
     :part-2 (apply + (take 3 sorted-elf-calories))}))

(let [test-data (slurp (io/resource "aoc-2022/01/test.dat"))
      input-data (slurp (io/resource "aoc-2022/01/input.dat"))]
  (assert (= {:part-1 24000 :part-2 45000} (answer test-data)))
  (assert (= {:part-1 70374 :part-2 204610} (answer input-data))))
