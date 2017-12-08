(ns aoc.day06.part1
  (:require [clojure.string :as str]))

(def test-data-1 [0 2 7 0])

(defn get-input-data
  []
  (let [input (slurp "resources/day06.part1.input")
        tokens (str/split input #"\s+")]
    (->> tokens
         (map #(Integer/parseInt %))
         (vec))))

(defn redistribute
  [banks start-index]
  (let [blocks (get banks start-index)]
    (loop [index (mod (inc start-index) (count banks))
           remaining blocks
           updated (assoc banks start-index 0)]
      (if (> remaining 0)
        (recur (mod (inc index) (count banks))
               (dec remaining)
               (update updated index inc))
        updated))))
               
(defn reallocate
  [banks]
  (loop [cycles 0
         configs #{}
         max-index (.indexOf banks (apply max banks))
         curr-config banks]
    (if (contains? configs curr-config)
      cycles
      (let [next-config (redistribute curr-config max-index)]
        (recur (inc cycles)
               (conj configs curr-config)
               (.indexOf next-config (apply max next-config))
               next-config)))))

(defn run
  []
  (reallocate (get-input-data)))
