(ns aoc.day05.part1
  (:require [clojure.string :as str]))

(def test-data-1
  {:input [0 3 0 1 -3]
   :output [2 5 0 1 -2]})

(defn get-input-data
  []
  (->> (slurp "resources/day05.part1.input")
       (str/split-lines)
       (map #(Integer/parseInt %))
       (vec)))

(defn process-jumps
  [jumps]
  (let [index 0
        steps (atom 0)]
    (loop [current-index index
           current-jumps jumps]
      (comment
      (println "curr idx: " current-index
               "steps: " @steps
               "jumps: " current-jumps))
      (if (>= current-index (count current-jumps))
        @steps
        (let [jump-value (get current-jumps current-index)
              new-index (+ current-index jump-value)]
          (comment
          (println "  jump: " jump-value " next inx: " new-index))
          (swap! steps inc)
          (recur new-index (assoc current-jumps current-index (inc jump-value))))))))

(defn run
  []
  (process-jumps (get-input-data)))

    
