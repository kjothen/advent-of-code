(ns aoc-2022.day-12
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; grid operations
(defn ->grid
  ([vs] {:width (count (first vs)) :height (count vs) :data vs})
  ([width height f]
   {:width width
    :height height
    :data (for [n (range (* width height))
                :let [x (rem n width)
                      y (quot n width)]]
            (f n width height x y))}))

(defn width [grid] (:width grid))
(defn height [grid] (:height grid))

(defn in-bounds?
  [grid [x y]]
  (and (<= 0 x) (<= 0 y) (< x (width grid)) (< y (height grid))))

(defn cell
  [grid [x y]]
  {:pre [(in-bounds? grid [x y])]}
  (nth (get-in grid [:data y]) x))

(defn nth->pos [grid n] [(rem n (width grid)) (quot n (width grid))])
(defn pos->nth [grid [x y]] (+ x (* (width grid) y)))

;; heightmap operations
(defn ->heightmap
  [s]
  (->> (str/split-lines s)
       (mapv #(mapv identity %))
       ->grid))

(defn char-pos
  [grid c]
  (reduce (fn [_ n]
            (let [pos (nth->pos grid n)]
              (when (= c (cell grid pos)) (reduced pos))))
          nil
          (range (* (width grid) (height grid)))))

(defn start-pos [grid] (char-pos grid \S))
(defn end-pos [grid] (char-pos grid \E))

(defn start? [grid pos] (= \S (cell grid pos)))
(defn exit? [grid pos] (= \E (cell grid pos)))
(defn lowest? [grid pos] (= \a (cell grid pos)))
(defn highest? [grid pos] (= \z (cell grid pos)))

(defn at-most-one-higher?
  [grid pos pos']
  (let [curr-val (int (cell grid pos))
        next-val (int (cell grid pos'))
        lowest-val (int \a)]
    (<= lowest-val next-val (inc curr-val))))

(defn step?
  [grid pos pos']
  (and (in-bounds? grid pos)
       (in-bounds? grid pos')
       (or (at-most-one-higher? grid pos pos')
           (and (start? grid pos) (lowest? grid pos'))
           (and (exit? grid pos') (highest? grid pos)))))

(defn upstep? [grid pos pos'] (step? grid pos pos'))
(defn downstep? [grid pos pos'] (step? grid pos' pos))

(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(defn steps
  [grid n fn-step?]
  (let [pos (nth->pos grid n)]
    (cond-> []
      (fn-step? grid pos (up pos)) (conj (pos->nth grid (up pos)))
      (fn-step? grid pos (down pos)) (conj (pos->nth grid (down pos)))
      (fn-step? grid pos (left pos)) (conj (pos->nth grid (left pos)))
      (fn-step? grid pos (right pos)) (conj (pos->nth grid (right pos))))))

(defn downsteps [grid n] (steps grid n downstep?))
(defn upsteps [grid n] (steps grid n upstep?))

(defn bfs
  [grid start fn-steps]
  (loop [paths (hash-map)
         marked (hash-set start)
         q (queue (list start))]
    (if (empty? q)
      paths
      (let [v (peek q)
            steps (set (fn-steps grid v))
            unmarked (set/difference steps marked)]
        (recur (reduce #(assoc %1 %2 v) paths unmarked)
               (set/union marked unmarked)
               (reduce #(conj %1 %2) (pop q) unmarked))))))

(defn path
  [paths start end]
  {:pre [(contains? paths end) (contains? (set (vals paths)) start)]}
  (loop [path []
         v end]
    (if (= start v)
      (do (prn (reverse (conj path start))) (reverse (conj path start)))
      (recur (conj path v) (get paths v)))))

(defn answer
  [s]
  (let [grid (->heightmap s)
        start (pos->nth grid (start-pos grid))
        end (pos->nth grid (end-pos grid))]
    {:part-1 (let [paths (bfs grid start upsteps)
                   path (path paths start end)
                   step-count (dec (count path))]
               step-count)
     :part-2 (let [paths (bfs grid end downsteps)
                   cell-count (* (width grid) (height grid))
                   lows (filter #(lowest? grid (nth->pos grid %1))
                                (range cell-count))]
               (reduce #(let [path (path paths end %2)
                              step-count (if (some? path) (dec (count path)) 0)]
                          (if (zero? step-count) %1 (min %1 step-count)))
                       Integer/MAX_VALUE
                       lows))}))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/12/test.dat")
      input-data (slurp-resource "aoc-2022/12/input.dat")]
  (assert {:part-1 31 :part-2 29} (answer test-data))
  (assert {:part-1 425 :part-2 418} (answer input-data)))
