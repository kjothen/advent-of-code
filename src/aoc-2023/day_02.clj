(ns aoc-2023.day-02
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn part-one [s] s)

(defn part-two [s] s)

(deftest day-02
  (testing "--- Part One ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/02/example.dat"))]
        (is (= data (part-one data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/02/input.dat"))]
        (is (= data (part-one data))))))
  (testing "--- Part Two ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/02/example.dat"))]
        (is (= data (part-two data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/02/input.dat"))]
        (is (= data (part-two data)))))))
