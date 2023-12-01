(ns aoc-2023.day-01
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn calibration-value
  [s]
  (let [digits (filter (fn [c] (Character/isDigit c)) s)]
    (parse-long (str (first digits) (last digits)))))

(defn first-spelled-digit
  [s digits]
  (re-find (re-pattern (str/join "|" digits)) s))

(defn last-spelled-digit
  [s digits]
  (str/reverse (re-find (re-pattern (str/join "|" (map str/reverse digits)))
                        (str/reverse s))))

;!zprint {:map {:nl-separator? false :flow true}}
(defn spelled-calibration-value
  [s]
  (let [digits {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                "six" 6 "seven" 7 "eight" 8 "nine" 9
                "1" 1 "2" 2 "3" 3 "4" 4 "5" 5
                "6" 6 "7" 7 "8" 8 "9" 9 "0" 0}]
    (parse-long (str (get digits (first-spelled-digit s (keys digits)))
                     (get digits (last-spelled-digit s (keys digits)))))))

(defn part-one
  [s]
  (->> (str/split-lines s)
       (map calibration-value)
       (apply +)))

(defn part-two
  [s]
  (->> (str/split-lines s)
       (map spelled-calibration-value)
       (apply +)))

(deftest day-01
  (testing "--- Part One ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/01/example-1.dat"))]
        (is (= 142 (part-one data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/01/input.dat"))]
        (is (= 54605 (part-one data))))))
  (testing "--- Part Two ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/01/example-2.dat"))]
        (is (= 281 (part-two data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/01/input.dat"))]
        (is (= 55429 (part-two data)))))))
