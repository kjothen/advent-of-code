(ns aoc-2022.day-02
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn ->encrypted-strategy
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #" "))
       (s/transform [s/ALL s/ALL] (comp char first))))

(defn decrypted-strategy
  [encrypted-strategy guide]
  (s/transform [s/ALL s/ALL] #(get guide %) encrypted-strategy))

(defn round-score
  [[them us] rules]
  (let [points {:rock 1 :paper 2 :scissors 3 :lose 0 :draw 3 :win 6}]
    (+ (get points us) (get points (get-in rules [them us])))))

(defn total-score
  [strategy rules]
  (reduce (fn [score hands] (+ score (round-score hands rules))) 0 strategy))

(defn answer
  [s]
  (let [encrypted-strategy (->encrypted-strategy s)
        guide {\A :rock \B :paper \C :scissors \X :rock \Y :paper \Z :scissors}
        guide' {\A :rock \B :paper \C :scissors \X :lose \Y :draw \Z :win}
        rules {:rock {:rock :draw :paper :win :scissors :lose}
               :paper {:rock :lose :paper :draw :scissors :win}
               :scissors {:rock :win :paper :lose :scissors :draw}}
        rules' (reduce-kv #(assoc %1 %2 (set/map-invert %3)) {} rules)]
    {:part-1 (total-score (decrypted-strategy encrypted-strategy guide) rules)
     :part-2 (total-score (decrypted-strategy encrypted-strategy guide')
                          rules')}))

(let [test-data (slurp (io/resource "aoc-2022/02/test.dat"))
      input-data (slurp (io/resource "aoc-2022/02/input.dat"))]
  (assert (= {:part-1 15 :part-2 12} (answer test-data)))
  (assert (= {:part-1 13675 :part-2 14184} (answer input-data))))
