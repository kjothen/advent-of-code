(ns aoc-2022.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL LAST transform]]))
(defn parse-uint [s] (Integer/parseUnsignedInt s))
(defn parse-motions
  [motions]
  (transform [ALL LAST]
             parse-uint
             (mapv (fn [motion] (string/split motion #"\s"))
                   (string/split-lines motions))))
(defn move-head
  [[x y] direction]
  (condp = direction
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))
(defn move-tail
  [[x y] [x' y']]
  (let [abs-diff (+ (abs (- x x')) (abs (- y y')))]
    (cond
      (and (= x x') (> y y') (> abs-diff 1)) [x (dec y)] ;N
      (and (= x x') (< y y') (> abs-diff 1)) [x (inc y)] ;S
      (and (< x x') (= y y') (> abs-diff 1)) [(inc x) y] ;W
      (and (> x x') (= y y') (> abs-diff 1)) [(dec x) y] ;E
      (and (< x x') (< y y') (> abs-diff 2)) [(inc x) (inc y)] ;SW
      (and (< x x') (> y y') (> abs-diff 2)) [(inc x) (dec y)] ;NW
      (and (> x x') (< y y') (> abs-diff 2)) [(dec x) (inc y)] ;SE
      (and (> x x') (> y y') (> abs-diff 2)) [(dec x) (dec y)] ;NE
      true [x y])))
(defn visit
  [motions num-tail-knots]
  (loop [motions motions
         H [0 0]
         T (repeat num-tail-knots [0 0])
         visited []
         num-moved 0]
    (if-not motions
      visited
      (let [[direction total-moves] (first motions)
            motions' (if (= (inc num-moved) total-moves) (next motions) motions)
            H' (move-head H direction)
            T' (next
                (reduce (fn [res t] (vec (conj res (move-tail t (last res)))))
                        [H']
                        T))
            visited' (conj visited (last T'))
            num-moved' (if (= (inc num-moved) total-moves) 0 (inc num-moved))]
        (recur motions' H' T' visited' num-moved')))))
(defn result
  [motions]
  {:part-1 (count (distinct (visit (parse-motions motions) 1)))
   :part-2 (count (distinct (visit (parse-motions motions) 9)))})
(let [test-data "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"
      input-data (slurp (io/resource "aoc-2022/09/input.dat"))]
  (assert (= {:part-1 13 :part-2 1} (result test-data)))
  (assert (= {:part-1 5513 :part-2 2427} (result input-data))))
