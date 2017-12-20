(ns aoc.day10.part1
  (:require [clojure.string :as str]))

(defn get-test-input
  []
  (let [input-string (str/trim (slurp "resources/day10.part1.input"))
        tokens (str/split input-string #",")
        lengths (map #(Integer/parseInt %) tokens)]
    lengths))

(defn subrev
  [xs start length]
  (if (<= (+ start length) (count xs))
    (let [sub (subvec xs start (+ start length))
          rev (reverse sub)]
      (lazy-cat
        (subvec xs 0 start)
        rev
        (subvec xs (+ start length) (count xs))))
    (let [xsxs (vec (concat xs xs))
          sub (vec (subvec xsxs start (+ start length)))
          rev (vec (reverse sub))
          overrun (- (+ start length) (count xs))
          sub-end (subvec rev 0 (- (count rev) overrun))
          sub-front (subvec rev (count sub-end) (+ (count sub-end) overrun))
          sub-middle (subvec xs (count sub-front) start)]
      (lazy-cat
        sub-front
        sub-middle
        sub-end))))

(defn knot-hash
  [lengths numbers]
  (loop [i 0
         xs numbers
         start 0
         skip 0]
    (if (not (< i (count lengths)))
      xs
      (let [curr-length (get lengths i)]
        (if (> curr-length (count xs))
          (throw (Exception. "invalid length"))
          (recur
            (inc i)
            (vec (subrev xs start curr-length))
            (mod (+ start curr-length skip) (count xs))
            (inc skip)))))))

(defn run
  []
  (knot-hash
    (vec (get-test-input))
    (vec (range 256))))
