(ns aoc.day03.part1)


(def test-data-1 {:input 1 :output 0})
(def test-data-2 {:input 12 :output 3})
(def test-data-3 {:input 23 :output 2})
(def test-data-4 {:input 1024 :output 31})

(def input 325489)

(defn perimiter-length
  [length]
  (* 4 (- length 1)))

;;only works for address values in the high partition
(defn spiral-memory
  [address]
  (let [dimension (Math/ceil (Math/sqrt address))
        max (* dimension dimension)
        mid (/ (- dimension 1) 2)
        mid-last (- max mid)
        dist-from-mid (Math/abs (- address mid-last))
        dist-from-center (+ dist-from-mid (/ (- dimension 1) 2))]
    dist-from-center))

(defn run
  []
  (spiral-memory input))
