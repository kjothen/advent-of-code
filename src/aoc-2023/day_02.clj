(ns aoc-2023.day-02
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn colour->freqs
  [turn-str]
  (reduce (fn [result colour-freq]
            (let [[freq colour] (str/split colour-freq #" ")]
              (assoc result colour (parse-long freq))))
          {}
          (str/split turn-str #", ")))

(defn id+turns
  [game-str]
  (let [id+turns (rest (re-find #"^Game ([0-9]+)\: (.*)$" game-str))
        id (parse-long (first id+turns))
        turns (mapv colour->freqs (str/split (second id+turns) #"; "))]
    [id turns]))

(defn possibles?
  [turns limits]
  (let [possible-turn-pred (fn [[colour freq]] (<= freq (get limits colour)))]
    (every? (fn [turn] (every? possible-turn-pred turn)) turns)))

(defn powers [turns] (apply * (vals (apply merge-with max turns))))

(defn game-strs [s] (map id+turns (str/split-lines s)))

(defn part-one
  [s]
  (let [limits {"red" 12 "green" 13 "blue" 14}
        possible-ids-pred (fn [[id turns]] (when (possibles? turns limits) id))]
    (apply + (keep possible-ids-pred (game-strs s)))))

(defn part-two [s] (apply + (map powers (map second (game-strs s)))))

(deftest day-02
  (testing "--- Part One ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/02/example.dat"))]
        (is (= 8 (part-one data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/02/input.dat"))]
        (is (= 3059 (part-one data))))))
  (testing "--- Part Two ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/02/example.dat"))]
        (is (= 2286 (part-two data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/02/input.dat"))]
        (is (= 65371 (part-two data)))))))
