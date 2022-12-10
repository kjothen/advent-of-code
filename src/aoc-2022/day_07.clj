(ns aoc-2022.day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn cd
  [path dir]
  (case dir
    "/" ["/"]
    ".." (vec (butlast path))
    (vec (conj path dir))))

(defn make-path-keys
  [path]
  (if (= ["/"] path) path (vec (butlast (interleave path (repeat :children))))))

(defn lines->tree
  [lines]
  (loop [tree {}
         lines (string/split-lines lines)
         path []]
    (if-not lines
      tree
      (let [line (first lines)
            parts (vec (string/split line #" "))
            cmd? (= "$" (first parts))
            cmd (when cmd? (second parts))
            path' (cond-> path
                    (and cmd? (= "cd" (second parts))) (cd (nth parts 2)))]
        (recur
         (if (not (or cmd? (= (first parts) "dir")))
           (let [file-entry {:size (parse-long (first parts))
                             :name (second parts)}]
             (update-in
              tree
              (make-path-keys path')
              (fn [m] (update m :files (fn [vs] (vec (conj vs file-entry)))))))
           tree)
         (next lines)
         path')))))

(defn file-sizes [files] (reduce (fn [acc file] (+ acc (:size file))) 0 files))

(defn tree->dir-sizes
  ([tree]
   (let [res (transient [])]
     (tree->dir-sizes res "" tree)
     (let [ret (persistent! res)] (apply hash-map (flatten ret)))))
  ([res dir tree]
   (mapv (fn [[k v]]
           (let [path (string/join (if (< 1 (count dir)) "/" "") [dir k])]
             (conj! res [path (file-sizes (:files v))])
             (when (:children v) (tree->dir-sizes res path (:children v)))))
         tree)))

(defn subdirs
  [parent-dir dir->sizes]
  (filterv (fn [[dir _]] (string/starts-with? dir parent-dir)) dir->sizes))

(defn subdir-sizes
  [dir->sizes]
  (reduce-kv (fn [m k v] (assoc m k (apply + (vals (subdirs k dir->sizes)))))
             {}
             dir->sizes))

(defn process
  [data]
  (let [tree (->> (lines->tree data)
                  tree->dir-sizes
                  subdir-sizes)
        total-size (get tree "/")
        free (- 70000000 total-size)
        target (- 30000000 free)]
    {:part-1 (apply + (filter #(<= % 100000) (vals tree)))
     :part-2 (apply min (filter #(>= % target) (vals tree)))}))

(let [test-data (slurp (io/resource "aoc-2022/07/test.dat"))
      input-data (slurp (io/resource "aoc-2022/07/input.dat"))]
  (assert (= {:part-1 1297159 :part-2 3866390} (process input-data)))
  (assert (= {:part-1 95437 :part-2 24933642} (process test-data))))
