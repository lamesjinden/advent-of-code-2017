(ns aoc.day02.part1
  (:require [clojure.pprint :as pp]
             [clojure.string :as str]))

(def test-data-1 { :input [[5 1 9 5]
                           [7 5 3]
                           [2 4 6 8]]
                  :output 18 })

(defn checksum-row
  [numbers]
  (let [min (apply min numbers)
        max (apply max numbers)
        difference (- max min)]
    difference))

(defn checksum
  [table]
  (->> table
       (map #(checksum-row %))
       (reduce +)))

(defn get-input-data
  []
  (->> (slurp "resources/day02.part1.input")
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [xs] (map (fn[x] (Integer/parseInt x)) xs)))))

(defn run
  []
  (checksum (get-input-data)))
