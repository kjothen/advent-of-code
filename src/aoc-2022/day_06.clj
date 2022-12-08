(ns aoc-2022.day-06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))
(defn marker
  [msg width]
  (+ width
     (reduce (fn [acc msg] (if (apply distinct? msg) (reduced acc) (inc acc)))
       0
       (partition width 1 msg))))
(assert (= 7 (marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4)))
(assert (= 5 (marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 4)))
(assert (= 6 (marker "nppdvjthqldpwncqszvftbrmjlhg" 4)))
(assert (= 10 (marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4)))
(assert (= 11 (marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4)))
(assert (= 19 (marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)))
(assert (= 23 (marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 14)))
(assert (= 23 (marker "nppdvjthqldpwncqszvftbrmjlhg" 14)))
(assert (= 29 (marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14)))
(assert (= 26 (marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14)))
(let [input-data (slurp (io/resource "aoc-2022/06/input.dat"))]
  (assert (= {:part-1 1034, :part-2 2472}
             {:part-1 (marker input-data 4), :part-2 (marker input-data 14)})))
