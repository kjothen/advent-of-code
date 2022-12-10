(ns aoc-2022.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.rpl.specter :refer [ALL LAST setval transform]]))

(defn process
  [instruction-lines fn-accumulator initial-value]
  (loop [x 1
         cycle 1
         result initial-value
         instructions (string/split-lines instruction-lines)]
    (if-not instructions
      result
      (let [[cmd arg] (string/split (first instructions) #" ")]
        (recur
         (condp = cmd "addx" (+ x (parse-long arg)) "noop" x)
         (condp = cmd "addx" (+ 2 cycle) "noop" (+ 1 cycle))
         (fn-accumulator result cmd (when (some? arg) (parse-long arg)) x cycle)
         (next instructions))))))

(defn signal-strengths
  [strength-cycles strengths cmd arg x cycle]
  (let [next-cycle (inc cycle)
        next-next-cycle (inc next-cycle)]
    (cond-> strengths
      (contains? strength-cycles next-cycle) (conj (* next-cycle x))
      (and (= cmd "addx") (contains? strength-cycles next-next-cycle))
      (conj (* next-next-cycle (+ x arg))))))

(defn sprite-position [x] #{(dec x) x (inc x)})

(defn pixel-position [cycle width] [(dec (mod cycle width)) (quot cycle width)])

(defn cathode-ray-tube
  [width display _ arg x cycle]
  (let [[xpos ypos] (pixel-position cycle width)
        [next-xpos next-ypos] (pixel-position (+ 1 cycle) width)
        [next-next-xpos next-next-ypos] (pixel-position (+ 2 cycle) width)
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
  (str/join \newline (map (partial apply str) display)))

(defn result
  [data]
  {:part-1
   (let [strength-cycles #{20 60 100 140 180 220}]
     (apply + (process data (partial signal-strengths strength-cycles) [])))
   :part-2 (let [[height width] [6 40]
                 display (vec (repeat height (vec (repeat width \.))))]
             (stringify-display
              (process data (partial cathode-ray-tube width) display)))})

(defn slurp-resource [n] (string/trimr (slurp (io/resource n))))

(let [test-input-data (slurp-resource "aoc-2022/10/test-input.dat")
      input-data (slurp-resource "aoc-2022/10/input.dat")
      test-output-data (slurp-resource "aoc-2022/10/test-output.dat")
      output-data (slurp-resource "aoc-2022/10/output.dat")]
  (assert (= {:part-1 13140 :part-2 test-output-data} (result test-input-data)))
  (assert (= {:part-1 10760 :part-2 output-data} (result input-data))))
