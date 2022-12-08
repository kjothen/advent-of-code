(ns aoc-2022.day-05
  (:require [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL MAP-VALS LAST select transform]]))
(defn parse-uint [s] (Integer/parseUnsignedInt s))
(defn repeat-str [n s] (string/join (repeat n s)))
(defn pad-str [n s pad] (string/join (take n (concat s (repeat pad)))))
(defn pad-lines [n lines pad] (map #(pad-str n % pad) lines))
(defn transpose [coll] (apply map vector coll))
(defn stack-lines->columns
  [lines]
  (let [line-width (inc (apply max (map count lines)))]
    (->> (pad-lines line-width (butlast lines) \space)
         (map (partial partition 4))
         (transform [ALL ALL] #(apply str %))
         transpose
         (map #(remove string/blank? %))
         (transform [ALL ALL] #(ffirst (next (re-matches #"\[(\w)\] " %))))
         (mapv (comp vec reverse)))))
(defn parse-stack-keys
  [lines]
  (mapv parse-uint (string/split (string/trim (last lines)) #"\s+")))
(defn parse-stacks
  [lines]
  (zipmap (parse-stack-keys lines) (stack-lines->columns lines)))
(defn parse-instructions
  [lines]
  (->> lines
       (map #(next (re-matches #"move (\d+) from (\d+) to (\d+)" %)))
       (map (fn [[num from to]] {:from from, :to to, :num num}))
       (transform [ALL MAP-VALS] parse-uint)))
(defn part-1
  [instructions stacks]
  (loop [instructions instructions
         stacks stacks
         moves 1]
    (if-not instructions
      stacks
      (let [{:keys [from to num]} (first instructions)
            from-val (last (get stacks from))]
        (recur (if (= num moves) (next instructions) instructions)
               (-> stacks
                   (update to (fn [stack] (conj stack from-val)))
                   (update from (fn [stack] (vec (drop-last stack)))))
               (if (= num moves) 1 (inc moves)))))))
(defn part-2
  [instructions stacks]
  (loop [instructions instructions
         stacks stacks]
    (if-not instructions
      stacks
      (let [{:keys [from to num]} (first instructions)
            from-vals (take-last num (get stacks from))]
        (recur (next instructions)
               (-> stacks
                   (update to (fn [stack] (vec (concat stack from-vals))))
                   (update from (fn [stack] (vec (drop-last num stack))))))))))
(defn read-stacks [stacks] (apply str (select [MAP-VALS LAST] (sort stacks))))
(defn process
  [data]
  (let [[stack-lines instruction-lines] (map string/split-lines
                                          (string/split data #"\n\n"))
        stacks (parse-stacks stack-lines)
        instructions (parse-instructions instruction-lines)]
    {:part-1 (read-stacks (part-1 instructions stacks)),
     :part-2 (read-stacks (part-2 instructions stacks))}))
(let
  [input-data (slurp (io/resource "aoc-2022/05/input.dat"))
   test-data
     "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"]
  (assert (= {:part-1 "CMZ", :part-2 "MCD"} (process test-data)))
  (assert (= {:part-1 "BZLVHBWQF", :part-2 "TDGJQTZSL"} (process input-data))))
