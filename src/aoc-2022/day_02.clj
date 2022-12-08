(ns aoc-2022.day-02
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL transform]]))
(defn parse
  [data]
  (->> (string/split-lines data)
       (map #(string/split % #" "))
       (transform [ALL ALL] (comp char first))))
(defn decrypt
  [data your-decryptor my-decryptor]
  (map (fn [[you me]] [(get your-decryptor you) (get my-decryptor me)]) data))
(defn score
  [plays points you me]
  (+ (get points me) (get points (get-in plays [you me]))))
(defn score-all
  [data score-fn plays points]
  (reduce (fn [res [you me]] (+ res (score-fn plays points you me))) 0 data))
(defn process
  [data rps ldw plays points]
  {:part-1 (score-all (decrypt data rps rps) score plays points),
   :part-2 (score-all (decrypt data rps ldw) score plays points)})
(let [rps {\A :rock, \B :paper, \C :scissors, \X :rock, \Y :paper, \Z :scissors}
      ldw {\X :lose, \Y :draw, \Z :win}
      points {:rock 1, :paper 2, :scissors 3, :lose 0, :draw 3, :win 6}
      play-1 {:rock {:rock :draw, :paper :win, :scissors :lose},
              :paper {:rock :lose, :paper :draw, :scissors :win},
              :scissors {:rock :win, :paper :lose, :scissors :draw}}
      play-2 (reduce-kv #(assoc %1 %2 (clojure.set/map-invert %3)) {} play-1)
      plays (merge-with merge play-1 play-2)
      test-data (parse "A Y\nB X\nC Z")
      input-data (parse (slurp (io/resource "aoc-2022/02/input.dat")))]
  (assert (= {:part-1 15, :part-2 12} (process test-data rps ldw plays points)))
  (assert (= {:part-1 13675, :part-2 14184}
             (process input-data rps ldw plays points))))
