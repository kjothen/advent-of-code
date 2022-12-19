(ns aoc-2022.day-13
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn ->packet-pairs
  [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)
       (map #(map edn/read-string %))))

(defn ->packets
  [s]
  (->> (str/split-lines s)
       (remove str/blank?)
       (map edn/read-string)))

(defn compare-packets
  [left right]
  (loop [left left
         right right]
    (let [l (first left)
          r (first right)]
      (cond
        (and (nil? l) (nil? r)) 0
        (and (nil? l) (some? r)) -1
        (and (some? l) (nil? r)) 1
        (and (int? l) (int? r) (not= l r)) (if (< l r) -1 1)
        :else
        (let [sequential-comparison (cond (and (sequential? l) (sequential? r))
                                          (compare-packets l r)
                                          (and (sequential? l) (int? r))
                                          (compare-packets l (conj (empty l) r))
                                          (and (int? l) (sequential? r))
                                          (compare-packets (conj (empty r) l) r)
                                          :else 0)]
          (if-not (zero? sequential-comparison)
            sequential-comparison
            (recur (rest left) (rest right))))))))

(defn ordered-packets
  [packet-pairs]
  (map inc
       (keep-indexed (fn [idx v] (when (<= v 0) idx))
                     (map (fn [[left right]] (compare-packets left right))
                          packet-pairs))))

(defn divider-packets
  [packets]
  (map inc
       (let [divider-packets [[[2]] [[6]]]]
         (->> (concat packets divider-packets)
              (sort compare-packets)
              (keep-indexed (fn [idx v]
                              (when (or (= (first divider-packets) v)
                                        (= (second divider-packets) v))
                                idx)))))))

(defn answer
  [s]
  {:part-1 (apply + (ordered-packets (->packet-pairs s)))
   :part-2 (apply * (divider-packets (->packets s)))})

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/13/test.dat")
      input-data (slurp-resource "aoc-2022/13/input.dat")]
  (assert (= {:part-1 13 :part-2 140} (answer test-data)))
  (assert (= {:part-1 5013 :part-2 25038} (answer input-data))))
