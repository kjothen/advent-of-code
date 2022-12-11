(ns aoc-2022.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn ->id [s] (parse-long (last (re-matches #"^Monkey (\d+):" s))))

(defn ->items
  [s]
  (let [items-str (last (re-matches #"^Starting items: ([0-9, ]+)" s))]
    (mapv parse-long (str/split items-str #", "))))

(defn ->operation
  [s]
  (let [[operator operand]
          (next (re-matches #"Operation: new = old ([\*+]) (\w+)" s))]
    {:operator (first operator),
     :operand (if (= "old" operand) :old (parse-long operand))}))

(defn ->divisor
  [s]
  (parse-long (last (re-matches #"Test: divisible by (\d+)" s))))

(defn ->condition
  [s]
  (let [[branch id] (next (re-matches #"If (\w+): throw to monkey (\d+)" s))]
    {(parse-boolean branch) (parse-long id)}))

(defn ->monkey
  [s]
  (loop [lines (str/split-lines s)
         monkey {}]
    (if-not lines
      monkey
      (let [line (str/trim (first lines))]
        (recur (next lines)
               (cond (str/starts-with? line "Monkey") (assoc monkey
                                                        :id (->id line))
                     (str/starts-with? line "Starting items")
                       (assoc monkey :items (->items line))
                     (str/starts-with? line "Operation")
                       (assoc monkey :operation (->operation line))
                     (str/starts-with? line "Test") (assoc monkey
                                                      :divisor (->divisor line))
                     (str/starts-with? line "If")
                       (update monkey :condition merge (->condition line))
                     :else monkey))))))

(defn item-operation
  [item {:keys [operator operand]}]
  ((if (= \* operator) * +) item (if (= :old operand) item operand)))

(defn throw-monkey-id
  [worry-level {:keys [divisor condition]}]
  (get condition (zero? (rem worry-level divisor))))

(defn worry-level
  [item {:keys [operation condition]} {:keys [worry-divisor worry-modulo]}]
  (long (cond-> (item-operation item operation)
          (some? worry-divisor) (/ worry-divisor)
          (some? worry-modulo) (rem worry-modulo))))

(defn turns
  [monkeys & opts]
  (loop [id 0
         result (vec monkeys)]
    (if (= id (count monkeys))
      result
      (let [monkey (nth result id)
            items (:items monkey)
            last-item? (<= (count items) 1)]
        (recur
          (if last-item? (inc id) id)
          (if-let [item (first items)]
            (let [worry-level (worry-level item monkey opts)
                  throw-id (throw-monkey-id worry-level monkey)]
              (-> result
                  (update-in [id :inspected] (fnil inc 0))
                  (update-in [id :items] next)
                  (update-in [throw-id :items] (comp vec conj) worry-level)))
            result))))))

(defn rounds
  [monkeys & {:keys [iterations], :as opts}]
  (loop [monkeys monkeys
         iterations iterations]
    (if (zero? iterations)
      monkeys
      (recur (turns monkeys opts) (dec iterations)))))

(defn inspected [monkeys] (apply * (take 2 (sort > (map :inspected monkeys)))))

(defn answer
  [s]
  (let [monkeys (->> (str/split s #"\n\n")
                     (map ->monkey))
        common-denominator (apply * (map :divisor monkeys))]
    {:part-1 (inspected (rounds monkeys {:iterations 20, :worry-divisor 3})),
     :part-2 (inspected (rounds monkeys
                                {:iterations 10000,
                                 :worry-modulo common-denominator}))}))

(defn slurp-resource [n] (str/trimr (slurp (io/resource n))))

(let [test-data (slurp-resource "aoc-2022/11/test.dat")
      input-data (slurp-resource "aoc-2022/11/input.dat")]
  (assert (= {:part-1 10605, :part-2 2713310158} (answer test-data)))
  (assert (= {:part-1 57838, :part-2 15050382231} (answer input-data))))
