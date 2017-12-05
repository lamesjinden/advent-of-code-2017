(ns aoc.day02.part2
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

(def test-data-1 {:input [[5 9 2 8]
                          [9 4 7 3]
                          [3 8 6 5]]
                  :output 9 })

(defn divisable?
  [dividend divisor]
  (= 0 (mod dividend divisor)))

(defn to-index-tuples
  [numbers]
  (let [num-vec (vec numbers)]
    (map-indexed (fn [i x] {:index i :value x}) num-vec)))

(defn row-to-division-pairs
  [row]
  (for [x (to-index-tuples row)
        y (to-index-tuples row)
        :when (not (= (:index x) (:index y)))]
    {:dividend (:value x)
     :divisor (:value y)}))

(defn get-divisible-pair
  [tuples]
  (first (filter #(divisable? (:dividend %) (:divisor %)) tuples)))

(defn checksum
  [table]
  (->> table
       (map (fn [row]
              (let [pairs (row-to-division-pairs row)
                    divisible-pair (get-divisible-pair pairs)]
                (/ (:dividend divisible-pair) (:divisor divisible-pair)))))
       (reduce +)))

(defn get-input-data
  []
  (->> (slurp "resources/day02.part2.input")
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [xs] (map (fn[x] (Integer/parseInt x)) xs)))))

(defn run
  []
  (checksum (get-input-data)))
