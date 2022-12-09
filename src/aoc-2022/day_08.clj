(ns aoc-2022.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL transform]]))
(defn cell [vs m n] (nth (nth vs m) n))
(defn make-range
  [vs m n view]
  (case view
    :up (reverse (range 0 m))
    :left (reverse (range 0 n))
    :right (range (inc n) (count vs))
    :down (range (inc m) (count vs))))
(defn make-coll
  [vs m n view]
  (let [r (make-range vs m n view)]
    (case view
      (:up :down) (reduce (fn [res m'] (conj res (cell vs m' n))) [] r)
      (:left :right) (reduce (fn [res n'] (conj res (cell vs m n'))) [] r))))
  ;; part-1
(defn view-visible?
  [vs m n view]
  (let [v (cell vs m n)
        c (make-coll vs m n view)]
    (reduce (fn [res v'] (if (<= v v') (reduced false) res)) true c)))
(defn cell-visible?
  [vs m n]
  (or (view-visible? vs m n :up)
      (view-visible? vs m n :left)
      (view-visible? vs m n :right)
      (view-visible? vs m n :down)))
(defn visibles
  [vs]
  (for [m (range 1 (dec (count vs)))
        n (range 1 (dec (count vs)))
        :when (cell-visible? vs m n)]
    (cell vs m n)))
  ;; part-2
(defn view-score
  [vs m n view]
  (let [v (cell vs m n)
        c (make-coll vs m n view)]
    (reduce (fn [res v'] (if (> v v') (inc res) (reduced (inc res)))) 0 c)))
(defn cell-score
  [vs m n]
  (* (view-score vs m n :up)
     (view-score vs m n :left)
     (view-score vs m n :down)
     (view-score vs m n :right)))
(defn scores
  [vs]
  (for [m (range 1 (dec (count vs)))
        n (range 1 (dec (count vs)))]
    (cell-score vs m n)))
  ;; process
(defn process
  [data]
  (let [vs (->> (string/split-lines data)
                (map (partial partition 1))
                (mapv (comp vec flatten))
                (transform [ALL ALL] #(parse-long (str %))))]
    {:part-1 (+ (count (visibles vs)) (- (* 4 (count vs)) 4))
     :part-2 (apply max (scores vs))}))
(let [test-data "30373
25512
65332
33549
35390"
      input-data (slurp (io/resource "aoc-2022/08/input.dat"))]
  (assert (= {:part-1 21 :part-2 8} (process test-data)))
  (assert (= {:part-1 1690 :part-2 535680} (process input-data))))
