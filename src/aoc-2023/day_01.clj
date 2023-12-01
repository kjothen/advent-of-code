(ns aoc-2023.day-01
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn calibration-value
  [s]
  (let [digits (filter (fn [c] (Character/isDigit c)) s)]
    (parse-long (str (first digits) (last digits)))))

(defn first-digit
  [s digits]
  (reduce (fn [[lowest item] digit]
            (let [idx (str/index-of s digit)]
              (cond (nil? idx)
                    [lowest item]

                    (zero? idx)
                    (reduced [idx digit])

                    (nil? lowest)
                    [idx digit]

                    (< idx lowest)
                    [idx digit]

                    :else
                    [lowest item])))
          [nil nil]
          digits))

(defn last-digit
  [s digits]
  (reduce (fn [[highest item] digit]
            (let [idx (str/last-index-of s digit)]
              (cond (nil? idx)
                    [highest item]

                    (= idx (- (count s) (count digit)))
                    (reduced [idx digit])

                    (nil? highest)
                    [idx digit]

                    (> idx highest)
                    [idx digit]

                    :else
                    [highest item])))
          [nil nil]
          digits))

;!zprint {:map {:nl-separator? false :flow true}}
(defn spelled-calibration-value
  [s]
  (let [digits {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                "six" 6 "seven" 7 "eight" 8 "nine" 9
                "1" 1 "2" 2 "3" 3 "4" 4 "5" 5
                "6" 6 "7" 7 "8" 8 "9" 9 "0" 0}]
    (parse-long (str (get digits (second (first-digit s (keys digits))))
                     (get digits (second (last-digit s (keys digits))))))))

(defn answer
  [s calibration-fn]
  (->> (str/split s #"\n")
       (map calibration-fn)
       (apply +)))

(deftest day-01
  (testing "test data"
    (let [data [(slurp (io/resource "aoc-2023/01/test-1.dat"))
                (slurp (io/resource "aoc-2023/01/test-2.dat"))]]
      (is (= {:part-1 142 :part-2 281}
             {:part-1 (answer (first data) calibration-value)
              :part-2 (answer (second data) spelled-calibration-value)}))))
  (testing "input data"
    (let [data (slurp (io/resource "aoc-2023/01/input.dat"))]
      (is (= {:part-1 54605 :part-2 55429}
             {:part-1 (answer data calibration-value)
              :part-2 (answer data spelled-calibration-value)})))))
