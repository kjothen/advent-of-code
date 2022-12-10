(ns aoc-2022.day-02
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]
            [com.rpl.specter :refer [ALL transform]]))

(defn strategy
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #" "))
       (transform [ALL ALL] (comp char first))))

(defn decrypt-strategy
  [encrypted-strategy guide]
  (transform [ALL ALL] #(get guide %) encrypted-strategy))

(defn score
  [[them us] rules]
  (let [points {:rock 1 :paper 2 :scissors 3 :lose 0 :draw 3 :win 6}]
    (+ (get points us) (get points (get-in rules [them us])))))

(defn total-score
  [strategy rules]
  (reduce (fn [res hands] (+ res (score hands rules))) 0 strategy))

(defn answer
  [s]
  (let [guide {\A :rock \B :paper \C :scissors \X :rock \Y :paper \Z :scissors}
        guide' {\A :rock \B :paper \C :scissors \X :lose \Y :draw \Z :win}
        rules {:rock {:rock :draw :paper :win :scissors :lose}
               :paper {:rock :lose :paper :draw :scissors :win}
               :scissors {:rock :win :paper :lose :scissors :draw}}
        rules' (reduce-kv #(assoc %1 %2 (clojure.set/map-invert %3)) {} rules)]
    {:part-1 (total-score (decrypt-strategy s guide) rules)
     :part-2 (total-score (decrypt-strategy s guide') rules')}))

(let [test-data (strategy (slurp (io/resource "aoc-2022/02/test.dat")))
      input-data (strategy (slurp (io/resource "aoc-2022/02/input.dat")))]
  (assert (= {:part-1 15 :part-2 12} (answer test-data)))
  (assert (= {:part-1 13675 :part-2 14184} (answer input-data))))
