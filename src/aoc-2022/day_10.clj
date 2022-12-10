(ns aoc-2022.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :refer [ALL LAST setval transform]]))
(defn process
  [instruction-lines fn-accumulator initial-value]
  (loop [x 1
         cycle-count 1
         result initial-value
         instructions (string/split-lines instruction-lines)]
    (if-not instructions
      result
      (let [[cmd arg] (string/split (first instructions) #" ")]
        (recur (condp = cmd "addx" (+ x (parse-long arg)) "noop" x)
               (condp = cmd "addx" (+ 2 cycle-count) "noop" (+ 1 cycle-count))
               (fn-accumulator result
                               cmd
                               (when (some? arg) (parse-long arg))
                               x
                               cycle-count)
               (next instructions))))))
(defn signal-strengths
  [strength-cycles strengths cmd arg x cycle-count]
  (cond-> strengths
    (contains? strength-cycles (inc cycle-count)) (conj (* (inc cycle-count) x))
    (and (= "addx" cmd) (contains? strength-cycles (+ 2 cycle-count)))
    (conj (* (+ 2 cycle-count) (+ x arg)))))
(defn sprite-position [x] #{(dec x) x (inc x)})
(defn pixel-position [cycle width] [(dec (mod cycle width)) (quot cycle width)])
(defn cathode-ray-tube
  [width display cmd arg x cycle-count]
  (let [[xpos ypos] (pixel-position cycle-count width)
        [next-xpos next-ypos] (pixel-position (+ 1 cycle-count) width)
        [next-next-xpos next-next-ypos] (pixel-position (+ 2 cycle-count) width)
        sprite-pos (sprite-position x)
        next-next-sprite-pos (when (some? arg) (sprite-position (+ x arg)))]
    (cond->> display
             (contains? sprite-pos xpos)
             (setval [ypos xpos] \#)
             (contains? sprite-pos next-xpos)
             (setval [next-ypos next-xpos] \#)
             (contains? next-next-sprite-pos next-next-xpos)
             (setval [next-next-ypos next-next-xpos] \#))))

(defn render-display [display] (doseq [x display] (println (apply str x))))
(defn stringify-display
  [display]
  (string/join \newline (map (partial apply str) display)))
(defn result
  [data]
  {:part-1
   (let [strength-cycles #{20 60 100 140 180 220}]
     (apply + (process data (partial signal-strengths strength-cycles) [])))
   :part-2 (let [[height width] [6 40]
                 display (vec (repeat height (vec (repeat width \.))))]
             (stringify-display
              (process data (partial cathode-ray-tube width) display)))})
(let
  [test-data (slurp (io/resource "aoc-2022/10/test-input.dat"))
   input-data (slurp (io/resource "aoc-2022/10/input.dat"))
   test-expected
   "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......###.
#######.......#######.......#######....."
   expected
   "####.###...##..###..#..#.####..##..#..#.
#....#..#.#..#.#..#.#..#.#....#..#.#..#.
###..#..#.#....#..#.####.###..#....####.
#....###..#.##.###..#..#.#....#.##.#..#.
#....#....#..#.#....#..#.#....#..#.#..#.
#....#.....###.#....#..#.#.....###.#..#."]
  (assert (= {:part-1 13140 :part-2 test-expected} (result test-data)))
  (assert (= {:part-1 10760 :part-2 expected} (result input-data))))
