(ns aoc-2022.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(defn ->heightmap
  [s]
  (let [vs (->> (str/split-lines s)
                reverse
                (mapv #(mapv identity %)))]
    {:rows (count vs) :cols (count (first vs)) :data vs}))

(defn in-bounds?
  [heightmap [x y]]
  (and (<= 0 x) (<= 0 y) (< x (:cols heightmap)) (< y (:rows heightmap))))

(defn cell [heightmap [x y]] (nth (get-in heightmap [:data y]) x))

(defn start-pos
  [heightmap]
  (first (for [x (range (:cols heightmap))
               y (range (:rows heightmap))
               :when (= \S (cell heightmap [x y]))]
           [x y])))

(defn start? [heightmap pos] (= \S (cell heightmap pos)))
(defn exit? [heightmap pos] (= \E (cell heightmap pos)))
(defn lowest? [heightmap pos] (= \a (cell heightmap pos)))
(defn highest? [heightmap pos] (= \z (cell heightmap pos)))

(defn at-most-one-higher?
  [heightmap [x y] [x' y']]
  (let [curr-val (int (cell heightmap [x y]))
        next-val (int (cell heightmap [x' y']))
        step-height (- next-val curr-val)]
    (or (zero? step-height) (zero? (dec step-height)))))

(defn visited?
  [heightmap [x y]]
  (contains? #{\U \D \L \R} (cell heightmap [x y])))

(defn step?
  [heightmap pos pos']
  (and (in-bounds? heightmap pos')
       (not (visited? heightmap pos'))
       (or (and (start? heightmap pos) (lowest? heightmap pos'))
           (and (exit? heightmap pos') (highest? heightmap pos))
           (at-most-one-higher? heightmap pos pos'))))

(declare walk-heightmap)

(defn step
  [heightmap [x y] direction steps]
  (let [visit (fn [c] (assoc-in heightmap [:data y x] c))
        marker (case direction
                 :up \U
                 :down \D
                 :left \L
                 :right \R)
        pos' (case direction
               :up [x (inc y)]
               :down [x (dec y)]
               :left [(dec x) y]
               :right [(inc x) y])]
    (if (step? heightmap [x y] pos')
      #(walk-heightmap (visit marker) pos' (cons [x y] steps))
      '())))

(defn walk-heightmap
  ([heightmap pos] (walk-heightmap heightmap pos '()))
  ([heightmap pos steps]
   (if (exit? heightmap pos)
     (list (cons pos steps))
     (concat (trampoline step heightmap pos :up steps)
             (trampoline step heightmap pos :down steps)
             (trampoline step heightmap pos :left steps)
             (trampoline step heightmap pos :right steps)))))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(def test-data (slurp-resource "aoc-2022/12/test.dat"))
(def input-data (slurp-resource "aoc-2022/12/input.dat"))

(comment
  (def h (->heightmap test-data))
  (def paths (walk-heightmap h (start-pos h)))
  (dec (apply min (map count paths))))
