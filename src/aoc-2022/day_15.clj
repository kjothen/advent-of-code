(ns aoc-2022.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn distance [[x y] [x' y']] (+ (abs (- x x')) (abs (- y y'))))

(def re
  #"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn ->beacon-sensors
  [s]
  (->> (str/split-lines s)
       (map #(re-matches re %))
       (map rest)
       (s/transform [s/ALL s/ALL] parse-long)
       (map #(split-at 2 %))
       (mapv #(let [sensor (vec (first %))
                    beacon (vec (second %))]
                (hash-map :sensor sensor
                          :beacon beacon
                          :reach (distance sensor beacon))))))

(defn ->sorted-beacon-sensors
  [beacon-sensors]
  (sort (fn [x y]
          (compare (- (get-in x [:sensor 1]) (get x :reach))
                   (- (get-in y [:sensor 1]) (get y :reach))))
        beacon-sensors))

(defn sort-intervals [vs] (sort (fn [a b] (compare (first a) (first b))) vs))
(defn merge-intervals
  [vs]
  (let [sorted (sort-intervals vs)]
    (reduce (fn [intervals [a' b']]
              (let [[a b] (last intervals)]
                (if (<= a' b)
                  (conj (drop-last intervals) [a (max b b')])
                  (conj intervals [a' b']))))
            [(first sorted)]
            (rest sorted))))

(defn sensor-row-interval
  [{:keys [sensor reach]} [x x'] row]
  (let [[sx sy] sensor
        y-dist (abs (- sy row))
        x-dist (- reach y-dist)]
    (when (< 0 x-dist) [(max x (- sx x-dist)) (min x' (+ sx x-dist))])))

(defn sensors-row-intervals
  [beacon-sensors search-interval row]
  (reduce
   (fn [intervals beacon-sensor]
     (let [interval (sensor-row-interval beacon-sensor search-interval row)]
       (if (nil? interval)
         intervals
         (let [intervals' (merge-intervals (concat [interval] intervals))]
           (if (= search-interval (first intervals'))
             (reduced [search-interval])
             intervals')))))
   []
   beacon-sensors))

(defn sensor-row-interval-size
  [beacon-sensors row]
  (let [[a b] (->> (sensors-row-intervals beacon-sensors
                                          [Integer/MIN_VALUE Integer/MAX_VALUE]
                                          row)
                   merge-intervals
                   first)]
    (inc (- b a))))

(defn row-beacons
  [beacon-sensors row]
  (count (s/select [s/ALL s/LAST (s/pred= row)]
                   (distinct (keep :beacon beacon-sensors)))))

(defn distress-beacon
  [beacon-sensors search-interval]
  (reduce
   (fn [_ row]
     (let [intervals (sensors-row-intervals beacon-sensors search-interval row)]
       (when-not (= search-interval (first intervals))
         (reduced [(-> intervals
                       merge-intervals
                       sort-intervals
                       first
                       second
                       inc) row]))))
   nil
   (range (first search-interval) (inc (second search-interval)))))

(defn answer
  [s row search-interval]
  (let [beacon-sensors (->sorted-beacon-sensors (->beacon-sensors s))]
    {:part-1 (- (sensor-row-interval-size beacon-sensors row)
                (row-beacons beacon-sensors row))
     :part-2 (let [[x y] (distress-beacon beacon-sensors search-interval)]
               (+ (* x 4000000) y))}))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/15/test.dat")
      input-data (slurp-resource "aoc-2022/15/input.dat")]
  (assert (= {:part-1 26 :part-2 56000011} (answer test-data 10 [0 20])))
  (assert (= {:part-1 5564017 :part-2 11558423398893}
             (answer input-data 2000000 [0 4000000]))))
