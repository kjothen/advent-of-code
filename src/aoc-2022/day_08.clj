(ns aoc-2022.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn ->trees
  [s]
  (->> (str/split-lines s)
       (map (partial partition 1))
       (mapv (comp vec flatten))
       (s/transform [s/ALL s/ALL] #(parse-long (str %)))))

(defn tree-height [trees m n] (nth (nth trees m) n))

(defn tree-range
  [trees m n view]
  (case view
    :up (reverse (range 0 m))
    :left (reverse (range 0 n))
    :right (range (inc n) (count trees))
    :down (range (inc m) (count trees))))

(defn tree-heights
  [trees m n view]
  (let [r (tree-range trees m n view)]
    (case view
      (:up :down)
      (reduce (fn [res m'] (conj res (tree-height trees m' n))) [] r)
      (:left :right)
      (reduce (fn [res n'] (conj res (tree-height trees m n'))) [] r))))

;; part-1
(defn visible-outside?
  ([trees m n]
   (or (visible-outside? trees m n :up)
       (visible-outside? trees m n :left)
       (visible-outside? trees m n :right)
       (visible-outside? trees m n :down)))
  ([trees m n view]
   (let [height (tree-height trees m n)
         heights (tree-heights trees m n view)]
     (reduce (fn [visible? height']
               (if (<= height height') (reduced false) visible?))
             true
             heights))))

(defn outside-visibles
  [trees]
  (count (for [m (range 1 (dec (count trees)))
               n (range 1 (dec (count trees)))
               :when (visible-outside? trees m n)]
           true)))

;; part-2
(defn scenic-score
  ([trees]
   (for [m (range 1 (dec (count trees)))
         n (range 1 (dec (count trees)))]
     (scenic-score trees m n)))
  ([trees m n]
   (* (scenic-score trees m n :up)
      (scenic-score trees m n :left)
      (scenic-score trees m n :down)
      (scenic-score trees m n :right)))
  ([trees m n view]
   (let [height (tree-height trees m n)
         heights (tree-heights trees m n view)]
     (reduce (fn [higher height']
               (if (> height height') (inc higher) (reduced (inc higher))))
             0
             heights))))

(defn answer
  [s]
  (let [trees (->trees s)]
    {:part-1 (+ (outside-visibles trees) (- (* 4 (count trees)) 4))
     :part-2 (apply max (scenic-score trees))}))

(let [test-data (slurp (io/resource "aoc-2022/08/test.dat"))
      input-data (slurp (io/resource "aoc-2022/08/input.dat"))]
  (assert (= {:part-1 21 :part-2 8} (answer test-data)))
  (assert (= {:part-1 1690 :part-2 535680} (answer input-data))))
