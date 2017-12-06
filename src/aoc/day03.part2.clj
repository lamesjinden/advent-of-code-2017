(ns aoc.day03.part2)

(def input 325489)

(defn perimiter-length
  [length]
  (* 4 (- length 1)))

(defn center-coord
  [length]
  {:x (/ (- length 1) 2)
   :y (/ (- length 1) 2)})


(defn side-length
  [index]
  (Math/ceil (Math/sqrt index)))

;;only works for address values in the high partition
(defn spiral-memory
  [address])

(defn run
  []
  (spiral-memory input))
