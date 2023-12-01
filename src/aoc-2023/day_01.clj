(ns aoc-2023.day-01
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(defn calibration-value
  [s]
  (let [digits (filter (fn [c] (Character/isDigit c)) s)]
    (parse-long (str (first digits) (last digits)))))

;!zprint {:map {:nl-separator? false :flow true}}
(defn spelled-calibration-value
  [s]
  (let [spelled-out-digits {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                            "six" 6 "seven" 7 "eight" 8 "nine" 9
                            "1" 1 "2" 2 "3" 3 "4" 4 "5" 5
                            "6" 6 "7" 7 "8" 8 "9" 9 "0" 0}
        first-digit (reduce (fn [[lowest item] digit]
                              (let [idx (str/index-of s digit)]
                                (cond (nil? idx) [lowest item]
                                      (nil? lowest) [idx digit]
                                      (< idx lowest) [idx digit]
                                      :else [lowest item])))
                            [nil nil]
                            (keys spelled-out-digits))
        last-digit (reduce (fn [[highest item] digit]
                             (let [idx (str/last-index-of s digit)]
                               (cond (nil? idx) [highest item]
                                     (nil? highest) [idx digit]
                                     (> idx highest) [idx digit]
                                     :else [highest item])))
                           [nil nil]
                           (keys spelled-out-digits))]
    (parse-long (str (get spelled-out-digits (second first-digit))
                     (get spelled-out-digits (second last-digit))))))

(defn answer [s calibration-fn]
  (->> (str/split s #"\n")
       (map calibration-fn)
       (apply +)))

(let [test-1-data (slurp (io/resource "aoc-2023/01/test-1.dat"))
      test-2-data (slurp (io/resource "aoc-2023/01/test-2.dat"))
      input-data (slurp (io/resource "aoc-2023/01/input.dat"))]
  (assert (= {:part-1 142 :part-2 281}
             {:part-1 (answer test-1-data calibration-value)
              :part-2 (answer test-2-data spelled-calibration-value)}))
  (assert (= {:part-1 54605 :part-2 55429}
             {:part-1 (answer input-data calibration-value)
              :part-2 (answer input-data spelled-calibration-value)})))
