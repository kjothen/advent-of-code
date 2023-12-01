(ns aoc-2023.day-NN
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn part-one [s] s)

(defn part-two [s] s)

(deftest day-NN
  (testing "--- Part One ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/NN/example.dat"))]
        (is (= data (part-one data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/NN/input.dat"))]
        (is (= data (part-one data))))))
  (testing "--- Part Two ---"
    (testing "Example Input"
      (let [data (slurp (io/resource "aoc-2023/NN/example.dat"))]
        (is (= data (part-two data)))))
    (testing "Puzzle Input"
      (let [data (slurp (io/resource "aoc-2023/NN/input.dat"))]
        (is (= data (part-two data)))))))
