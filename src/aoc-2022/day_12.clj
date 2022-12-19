(ns aoc-2022.day-12
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; grid
(defn ->grid
  ([vs] {:width (count (first vs)) :height (count vs) :data vs})
  ([width height f]
   {:width width
    :height height
    :data (for [n (range (* width height))
                :let [x (rem n width)
                      y (quot n width)]]
            (f n width height x y))}))

(defn width [grid] (:width grid))
(defn height [grid] (:height grid))

(defn cell [grid [x y]] (nth (get-in grid [:data y]) x))

(defn nth->pos [grid n] [(rem n (width grid)) (quot n (width grid))])
(defn pos->nth [grid [x y]] (+ x (* (width grid) y)))

(defn in-bounds?
  [grid [x y]]
  (and (<= 0 x) (<= 0 y) (< x (width grid)) (< y (height grid))))

;; heightmap
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
(defn lowest? [grid pos] (= \a (cell grid pos)))

(defn cell->int
  [grid pos]
  (let [c (cell grid pos)] (int (condp = c \S \a \E \z c))))

(defn at-most-one-higher?
  [grid pos pos']
  (<= (- (cell->int grid pos') (cell->int grid pos)) 1))

(defn neighbour?
  [grid pos pos']
  (and (in-bounds? grid pos') (at-most-one-higher? grid pos pos')))

(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(defn neighbours
  [grid n]
  (let [pos (nth->pos grid n)]
    (cond-> []
      (neighbour? grid pos (up pos)) (conj (pos->nth grid (up pos)))
      (neighbour? grid pos (down pos)) (conj (pos->nth grid (down pos)))
      (neighbour? grid pos (left pos)) (conj (pos->nth grid (left pos)))
      (neighbour? grid pos (right pos)) (conj (pos->nth grid (right pos))))))

;; search
(defn bfs
  [grid start fn-neighbours]
  (loop [end->start (hash-map)
         marked (hash-set start)
         q (queue (list start))]
    (if (empty? q)
      end->start
      (let [v (peek q)
            neighbours (set (fn-neighbours grid v))
            unmarked (set/difference neighbours marked)]
        (recur (reduce #(assoc %1 %2 v) end->start unmarked)
               (set/union marked unmarked)
               (reduce #(conj %1 %2) (pop q) unmarked))))))

(defn path
  [end->start start end]
  (when (contains? end->start end)
    (loop [path []
           v end]
      (if (= start v)
        (reverse (conj path start))
        (recur (conj path v) (get end->start v))))))

;; answer
(defn answer
  [s]
  (let [grid (->heightmap s)
        end (pos->nth grid (end-pos grid))]
    {:part-1 (let [start (pos->nth grid (start-pos grid))
                   end->start (bfs grid start neighbours)
                   path (path end->start start end)]
               (dec (count path)))
     :part-2 (let [starts (filter #(lowest? grid (nth->pos grid %1))
                                  (range (* (width grid) (height grid))))
                   paths (reduce #(let [end->start (bfs grid %2 neighbours)
                                        path (path end->start %2 end)]
                                    (cond-> %1 (some? path) (conj path)))
                                 []
                                 starts)]
               (dec (apply min (map count paths))))}))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/12/test.dat")
      input-data (slurp-resource "aoc-2022/12/input.dat")]
  (assert (= {:part-1 31 :part-2 29} (answer test-data)))
  (assert (= {:part-1 425 :part-2 418} (answer input-data))))
