(ns aoc-2022.day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :as s]))

(def rock \#)
(def air \.)
(def sand \o)

(defn ->rock-paths
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #" -> "))
       (s/transform [s/ALL s/ALL] #(str/split % #","))
       (s/transform [s/ALL s/ALL s/ALL] parse-long)))

(defn bounding-rect
  [rock-paths]
  (let [xs (s/select [s/ALL s/ALL s/FIRST] rock-paths)
        ys (s/select [s/ALL s/ALL s/LAST] rock-paths)]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn cave
  [[x y x' y']]
  (let [height (inc (max y y'))
        width (inc (- (max x x') (min x x')))]
    {:offset (min x x')
     :width width
     :height height
     :data (vec (for [_ (range height)] (vec (repeat width air))))}))

(defn inf-bottom-cave
  [[x y x' y']]
  (let [cave (cave [x y x' y'])
        height (+ 2 (:height cave))
        width (+ (:width cave) height height)
        offset (- (:offset cave) height)]
    {:offset offset
     :width width
     :height height
     :data (vec (for [i (range height)]
                  (vec (repeat width (if (= height (inc i)) rock air)))))}))

(defn pad-str [n s pad] (str/join (take n (concat s (repeat pad)))))
(defn print-cave
  [cave]
  (map-indexed #(println (pad-str 3 (str %1) \space) (apply str %2))
               (:data cave)))

(defn value [cave [x y]] (get-in cave [:data y (- x (:offset cave))]))
(defn set-value [cave [x y] v] (assoc-in cave [:data y (- x (:offset cave))] v))

(defn in-cave?
  [cave [x y]]
  (and (<= (:offset cave) x (dec (+ (:width cave) (:offset cave))))
       (<= 0 y (dec (:height cave)))))

(defn ->rock-formation
  [cave [[x y] [x' y']]]
  (loop [cave cave
         xs (range (min x x') (inc (max x x')))]
    (if (empty? xs)
      cave
      (recur
       (loop [cave cave
              ys (range (min y y') (inc (max y y')))]
         (if (empty? ys)
           cave
           (recur (let [x (first xs) y (first ys)] (set-value cave [x y] rock))
                  (rest ys))))
       (rest xs)))))

(defn rock-cave-formations
  [cave formations]
  (reduce (fn [cave formation] (->rock-formation cave formation))
          cave
          formations))

(defn rock-cave
  [cave rock-paths]
  (reduce (fn [cave formations] (rock-cave-formations cave formations))
          cave
          (s/transform [s/ALL] #(partition 2 1 %) rock-paths)))

(defn down [[x y]] [x (inc y)])
(defn down-left [[x y]] [(dec x) (inc y)])
(defn down-right [[x y]] [(inc x) (inc y)])

(defn free? [cave pos] (or (not (in-cave? cave pos)) (= air (value cave pos))))
(defn down? [cave pos] (free? cave (down pos)))
(defn down-left? [cave pos] (free? cave (down-left pos)))
(defn down-right? [cave pos] (free? cave (down-right pos)))

(defn sand-drop-position
  [cave start-pos]
  (loop [sand-pos start-pos]
    (let [next-sand-pos (cond (down? cave sand-pos) (down sand-pos)
                              (down-left? cave sand-pos) (down-left sand-pos)
                              (down-right? cave sand-pos) (down-right sand-pos)
                              :else sand-pos)]
      (if-not (in-cave? cave next-sand-pos)
        nil
        (if (= sand-pos next-sand-pos)
          (if (and (= sand-pos start-pos) (= sand (value cave sand-pos)))
            nil
            sand-pos)
          (recur next-sand-pos))))))

(defn sand-units
  [cave start-pos]
  (loop [cave cave
         units 0]
    (let [pos (sand-drop-position cave start-pos)]
      (if (nil? pos)
        {:cave cave :units units}
        (recur (set-value cave pos sand) (inc units))))))

(defn answer
  [s]
  (let [start-pos [500 0]
        rock-paths (->rock-paths s)
        rect (bounding-rect rock-paths)
        cave (cave rect)
        inf-bottom-cave (inf-bottom-cave rect)
        units (fn [cave]
                (:units (sand-units (rock-cave cave rock-paths) start-pos)))]
    {:part-1 (units cave) :part-2 (units inf-bottom-cave)}))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/14/test.dat")
      input-data (slurp-resource "aoc-2022/14/input.dat")]
  (assert (= {:part-1 24 :part-2 93} (answer test-data)))
  (assert (= {:part-1 665 :part-2 25434} (answer input-data))))
