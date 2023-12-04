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
  (let [[_ _ numbers-str] (re-find #"^Card(?:[ ]+)([0-9]+)\: (.*)$" line)
        [winning-numbers-str card-numbers-str] (str/split numbers-str #"\|")
        card-numbers (number-strs->numbers card-numbers-str)
        winning-numbers (number-strs->numbers winning-numbers-str)]
    (count (clojure.set/intersection card-numbers winning-numbers))))

(defn card-worth
  [line]
  (let [matches (card-matches line)]
    (if (pos? matches) (int (math/pow 2 (dec matches))) 0)))

(defn part-one [s] (apply + (map card-worth (str/split-lines s))))

(defn part-two
  [s]
  (let [matches (map card-matches (str/split-lines s))
        idx-max (dec (count matches))]
    (loop [winnings {}
           idx 0]
      (if (> idx idx-max)
        (apply + (vals winnings))
        (let [wins (nth matches idx)]
          (prn winnings)
          (recur (reduce (fn [result win-idx]
                           (update-in result
                                      (str win-idx)
                                      (fn [curr] (inc (or curr 0)))))
                         winnings
                         (when (pos? wins)
                           (range (inc idx) (min idx-max (+ (inc idx) wins)))))
                 (inc idx)))))))

(comment
  (def data (slurp (io/resource "aoc-2023/04/example.dat")))
  (def lines (str/split-lines data))
  (apply + (map card-worth lines))

  (part-two data)

  ;
)

(deftest day-04
  (testing "Part One - Example"
    (let [data (slurp (io/resource "aoc-2023/04/example.dat"))]
      (is (= 13 (part-one data)))))
  (testing "Part One - Puzzle"
    (let [data (slurp (io/resource "aoc-2023/04/input.dat"))]
      (is (= 19135 (part-one data)))))
  (testing "Part Two - Example"
    (let [data (slurp (io/resource "aoc-2023/04/example.dat"))]
      (is (= data (part-two data)))))
  (testing "Part Two - Puzzle"
    (let [data (slurp (io/resource "aoc-2023/04/input.dat"))]
      (is (= data (part-two data))))))
