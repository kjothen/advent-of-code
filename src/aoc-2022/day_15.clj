(ns aoc-2022.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(def re
  #"^Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn distance [[x y] [x' y']] (+ (abs (- x x')) (abs (- y y'))))

(defn ->positions
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
                          :distance (distance sensor beacon))))))

(defn ->bounding-rect
  [positions]
  (let [xs (concat (s/select [s/ALL :beacon s/FIRST] positions)
                   (s/select [s/ALL :sensor s/FIRST] positions))
        ys (concat (s/select [s/ALL :beacon s/LAST] positions)
                   (s/select [s/ALL :sensor s/LAST] positions))]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn ->sensing-row-bounding-rect
  [bounds positions y]
  (reduce (fn [bounds position]
            (let [[bx by bx' by'] bounds
                  [sx sy] (:sensor position)
                  sd (:distance position)
                  xd (distance [bx y] [sx sy])
                  x (if (< xd sd) (- bx (- sd xd)) bx)
                  xd' (distance [bx' y] [sx sy])
                  x' (if (< xd' sd) (+ bx' (- sd xd')) bx')]
              [x by x' by']))
          bounds
          positions))

(defn sensor-covers
  [positions y]
  (let [bounds (->bounding-rect positions)
        sensing-bounds (->sensing-row-bounding-rect bounds positions y)
        sensor-covers
        (count
         (distinct (for [y [y]
                         x (let [[x y x' y'] sensing-bounds] (range x (inc x')))
                         position positions
                         :when (<= (distance (:sensor position) [x y])
                                   (:distance position))]
                     x)))
        beacon-covers (count (reduce #(if (= y (get-in %2 [:beacon 1]))
                                        (conj %1 (get-in %2 [:beacon]))
                                        %1)
                                     #{}
                                     positions))]
    (- sensor-covers beacon-covers)))

(defn answer
  [s y]
  (let [positions (->positions s)] {:part-1 (sensor-covers positions y)}))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/15/test.dat")
      input-data (slurp-resource "aoc-2022/15/input.dat")]
  (assert (= {:part-1 26} (answer test-data 10)))
  (assert (= {:part-1 5564017} (answer input-data 2000000))))
