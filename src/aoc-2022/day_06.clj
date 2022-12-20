(ns aoc-2022.day-06
  (:require [clojure.java.io :as io]))

(defn start-marker
  [msg width]
  (+ width
     (reduce (fn [acc msg] (if (apply distinct? msg) (reduced acc) (inc acc)))
             0
             (partition width 1 msg))))

(defn start-packet-marker [msg] (start-marker msg 4))
(defn start-message-marker [msg] (start-marker msg 14))

(defn answer
  [s]
  {:part-1 (start-packet-marker s) :part-2 (start-message-marker s)})

(let [input-data (slurp (io/resource "aoc-2022/06/input.dat"))]
  (assert (= 7 (start-packet-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (assert (= 5 (start-packet-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (assert (= 6 (start-packet-marker "nppdvjthqldpwncqszvftbrmjlhg")))
  (assert (= 10 (start-packet-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (assert (= 11 (start-packet-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))
  (assert (= 19 (start-message-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (assert (= 23 (start-message-marker "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (assert (= 23 (start-message-marker "nppdvjthqldpwncqszvftbrmjlhg")))
  (assert (= 29 (start-message-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (assert (= 26 (start-message-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))
  (assert (= {:part-1 1034 :part-2 2472} (answer input-data))))
