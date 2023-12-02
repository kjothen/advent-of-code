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
  (let [regex #"^Game ([0-9]+)\: (.*)$"
        id+turns (rest (first (re-seq regex game-str)))
        id (parse-long (first id+turns))
        turns (mapv colour->freqs (str/split (second id+turns) #"; "))]
    [id turns]))

(defn turns-possible?
  [turns limits]
  (reduce (fn [_ turn]
            (if (false? (reduce-kv (fn [_ colour freq]
                                     (if (> freq (get limits colour))
                                       (reduced false)
                                       true))
                                   true
                                   turn))
              (reduced false)
              true))
          true
          turns))

(defn turns-power [turns] (apply * (vals (apply merge-with max turns))))

(defn part-one
  [s]
  (let [games (map id+turns (str/split-lines s))
        limits {"red" 12 "green" 13 "blue" 14}]
    (reduce (fn [result [id turns]]
              (prn id)
              (if (turns-possible? turns limits) (+ id result) result))
            0
            games)))

(defn part-two
  [s]
  (let [games (map id+turns (str/split-lines s))]
    (apply + (map turns-power (map second games)))))

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
