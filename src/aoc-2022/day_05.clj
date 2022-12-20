(ns aoc-2022.day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn repeat-str [n s] (str/join (repeat n s)))
(defn pad-str [n s pad] (str/join (take n (concat s (repeat pad)))))
(defn pad-lines [n lines pad] (map #(pad-str n % pad) lines))
(defn transpose [coll] (apply map vector coll))

(defn ->instructions
  [ss]
  (->> ss
       (map #(next (re-matches #"move (\d+) from (\d+) to (\d+)" %)))
       (map (fn [[num from to]] {:from from :to to :num num}))
       (s/transform [s/ALL s/MAP-VALS] parse-long)))

(defn ->stack-crates
  [ss]
  (let [line-width (inc (apply max (map count ss)))]
    (->> (pad-lines line-width ss \space)
         (map (partial partition 4))
         (s/transform [s/ALL s/ALL] #(apply str %))
         transpose
         (map #(remove str/blank? %))
         (s/transform [s/ALL s/ALL]
                      #(ffirst (next (re-matches #"\[(\w)\] " %))))
         (mapv (comp vec reverse)))))

(defn ->stack-labels [s] (mapv parse-long (str/split (str/trim s) #"\s+")))

(defn ->stacks
  [ss]
  (zipmap (->stack-labels (last ss)) (->stack-crates (butlast ss))))

(defn ->stacks+instructions
  [s]
  (let [ss (map str/split-lines (str/split s #"\n\n"))]
    [(->stacks (first ss)) (->instructions (second ss))]))

(defn cratemover-9000-arrangement
  [stacks instructions]
  (loop [stacks stacks
         instructions instructions
         moves 1]
    (if-not instructions
      stacks
      (let [{:keys [from to num]} (first instructions)
            from-val (last (get stacks from))]
        (recur (-> stacks
                   (update to (fn [stack] (conj stack from-val)))
                   (update from (fn [stack] (vec (drop-last stack)))))
               (if (= num moves) (next instructions) instructions)
               (if (= num moves) 1 (inc moves)))))))

(defn cratemover-9001-arrangement
  [stacks instructions]
  (loop [stacks stacks
         instructions instructions]
    (if-not instructions
      stacks
      (let [{:keys [from to num]} (first instructions)
            from-vals (take-last num (get stacks from))]
        (recur (-> stacks
                   (update to (fn [stack] (vec (concat stack from-vals))))
                   (update from (fn [stack] (vec (drop-last num stack)))))
               (next instructions))))))

(defn stack-tops
  [stacks]
  (apply str (s/select [s/MAP-VALS s/LAST] (sort stacks))))

(defn answer
  [s]
  (let [[stacks instructions] (->stacks+instructions s)]
    {:part-1 (stack-tops (cratemover-9000-arrangement stacks instructions))
     :part-2 (stack-tops (cratemover-9001-arrangement stacks instructions))}))

(let [input-data (slurp (io/resource "aoc-2022/05/input.dat"))
      test-data (slurp (io/resource "aoc-2022/05/test.dat"))]
  (assert (= {:part-1 "CMZ" :part-2 "MCD"} (answer test-data)))
  (assert (= {:part-1 "BZLVHBWQF" :part-2 "TDGJQTZSL"} (answer input-data))))
