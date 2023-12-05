(ns aoc-2023.day-04
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.set]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn number-strs->numbers
  [s]
  (set (map parse-long (str/split (str/trim s) #"\s+"))))

(defn card-matches
  [line]
  (let [[_ winning-numbers-str card-numbers-str] (str/split line #":|\|")
        card-numbers (number-strs->numbers card-numbers-str)
        winning-numbers (number-strs->numbers winning-numbers-str)]
    (count (clojure.set/intersection card-numbers winning-numbers))))

(defn card-worth
  [line]
  (let [matches (card-matches line)]
    (if (pos? matches) (int (math/pow 2 (dec matches))) 0)))

(defn card-wins
  [lines]
  (let [matches (map card-matches lines)
        idx-max (dec (count matches))]
    (loop [cards (vec (repeat (inc idx-max) 1))
           idx 0]
      (if (= idx idx-max)
        cards
        (let [wins (nth matches idx)
              copies (nth cards idx)
              winning-cards (range (inc idx) (+ (inc idx) wins))]
          (recur (reduce (fn [result win-idx]
                           (update result win-idx (partial + copies)))
                         cards
                         winning-cards)
                 (inc idx)))))))

(defn part-one [s] (apply + (map card-worth (str/split-lines s))))

(defn part-two [s] (apply + (card-wins (str/split-lines s))))

(deftest day-04
  (testing "Part One - Example"
    (let [data (slurp (io/resource "aoc-2023/04/example.dat"))]
      (is (= 13 (part-one data)))))
  (testing "Part One - Puzzle"
    (let [data (slurp (io/resource "aoc-2023/04/input.dat"))]
      (is (= 19135 (part-one data)))))
  (testing "Part Two - Example"
    (let [data (slurp (io/resource "aoc-2023/04/example.dat"))]
      (is (= 30 (part-two data)))))
  (testing "Part Two - Puzzle"
    (let [data (slurp (io/resource "aoc-2023/04/input.dat"))]
      (is (= 5704953 (part-two data))))))
