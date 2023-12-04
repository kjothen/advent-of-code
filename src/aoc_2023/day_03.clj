(ns aoc-2023.day-03
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(defn spans
  [schematic]
  (loop [lines schematic
         i 0
         j 0
         start nil
         stop nil
         spans []]
    (let [line (first lines)
          digit? (Character/isDigit (nth line i))
          eol? (= i (dec (count line)))]
      (if (and eol? (zero? (dec (count lines))))
        spans
        (recur (if eol? (rest lines) lines)
               (if eol? 0 (inc i))
               (if eol? (inc j) j)
               (cond (and (nil? start) digit?) [i j]
                     (and (some? start) (not digit?)) nil
                     :else start)
               (when digit? [i j])
               (if (and (not digit?) (some? start) (some? stop))
                 (conj spans [start stop])
                 spans))))))

(defn bounds
  [spans]
  (reduce (fn [res [[x1 y1] [x2 y2]]]
            (conj res [[(dec x1) (dec y1)] [(inc x2) (inc y2)]]))
          []
          spans))

(defn rects
  [bounds]
  (mapv (fn [[[x1 y1] [x2 y2]]]
          (partition 2 1 [[x1 y1] [x2 y1] [x2 y2] [x1 y2] [x1 y1]]))
        bounds))

(defn points
  [xmax ymax rects]
  (map (fn [rect]
         (->> (keep
               (fn [[[x1 y1] [x2 y2]]]
                 (cond (and (= x1 x2) (<= 0 x1 xmax))
                       (keep (fn [y] (when-not (or (neg? y) (> y ymax)) [x1 y]))
                             (range (min y1 y2) (inc (max y1 y2))))
                       (<= 0 y1 ymax)
                       (keep (fn [x] (when-not (or (neg? x) (> x xmax)) [x y1]))
                             (range (min x1 x2) (inc (max x1 x2))))
                       :else nil))
               rect)
              (filter not-empty)
              flatten
              (partition 2)))
       rects))

(defn numbers
  [schematic spans]
  (map (fn [[[x1 y] [x2 _]]] (parse-long (subs (nth schematic y) x1 (inc x2))))
       spans))

(defn is-symbol?
  [schematic [x y]]
  (let [c (nth (nth schematic y) x)]
    (and (not= c \.) (not (Character/isDigit c)))))

(defn adjacents
  [schematic points]
  (keep-indexed (fn [idx item]
                  (when (some (partial is-symbol? schematic) item) idx))
                points))

(defn is-gear? [schematic [x y]] (let [c (nth (nth schematic y) x)] (= c \*)))

(defn gears
  [schematic]
  (for [y (range (count schematic))
        x (range (count (first schematic)))
        :when (is-gear? schematic [x y])]
    [x y]))

(defn in? [xs x] (some (partial = x) xs))

(defn gear-spans
  [gears points]
  (map-indexed (fn [gear-idx gear]
                 (keep-indexed (fn [rect-idx item]
                                 (when (in? item gear) [gear-idx rect-idx]))
                               points))
               gears))

(defn two-gear-spans [gear-spans] (filter (fn [x] (= 2 (count x))) gear-spans))

(defn part-one
  [s]
  (let [schematic (str/split-lines s)
        ymax (dec (count schematic))
        xmax (dec (count (first schematic)))
        ss (spans schematic)
        as (->> ss
                bounds
                rects
                (points xmax ymax)
                (adjacents schematic))
        ns (numbers schematic ss)]
    (apply + (map (fn [a] (nth ns a)) as))))

(defn part-two
  [s]
  (let [schematic (str/split-lines s)
        ymax (dec (count schematic))
        xmax (dec (count (first schematic)))
        ss (spans schematic)
        gs (gears schematic)
        gss (->> ss
                 bounds
                 rects
                 (points xmax ymax)
                 (gear-spans gs)
                 two-gear-spans)
        ns (numbers schematic ss)]
    (apply +
           (map (fn [[[_ span-1] [_ span-2]]]
                  (* (nth ns span-1) (nth ns span-2)))
                gss))))

(deftest day-03
  (testing "Part One - Example"
    (let [data (slurp (io/resource "aoc-2023/03/example.dat"))]
      (is (= 4361 (part-one data)))))
  (testing "Part One - Puzzle"
    (let [data (slurp (io/resource "aoc-2023/03/input.dat"))]
      (is (= 526404 (part-one data)))))
  (testing "Part Two - Example"
    (let [data (slurp (io/resource "aoc-2023/03/example.dat"))]
      (is (= 467835 (part-two data)))))
  (testing "Part Two - Puzzle"
    (let [data (slurp (io/resource "aoc-2023/03/input.dat"))]
      (is (= 84399773 (part-two data))))))
