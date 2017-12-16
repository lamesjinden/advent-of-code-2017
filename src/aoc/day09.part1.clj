(ns aoc.day09.part1
  (:require [clojure.string :as str]))

(def test-input-1 {:input "{}"
                   :groups 1
                   :score 1})

(def test-input-2 {:input "{{{}}}"
                   :groups 3
                   :score 6})

(def test-input-3 {:input "{{},{}}"
                   :groups 3
                   :score 5})

(def test-input-4 {:input "{{{},{},{{}}}}"
                   :groups 6
                   :score 16 })

(def test-input-5 {:input "{<{},{},{{}}>}"
                   :groups 1
                   :score 1})

(def test-input-6 {:input "{<a>,<a>,<a>,<a>}"
                   :groups 1
                   :score 1})

(def test-input-7 {:input "{{<a>},{<a>},{<a>},{<a>}}"
                   :groups 5
                   :score 9})

(def test-input-8 {:input "{{<!>},{<!>},{<!>},{<a>}}"
                   :groups 2
                   :score 3})

(defn parse-garbage
  [garbage index]
  (let [i (atom index)]
    (loop [closed? false]
      (when (not closed?)
        (let [char (get garbage @i)]
          (cond
            (= char \>) (do (swap! i inc)
                            (recur true))
            (= char \!) (do (swap! i #(+ % 2))
                            (recur false))
            :else (do (swap! i inc)
                      (recur false))))))
    @i))

(defn traverse
  [input-string]
  (let [input-vec (vec input-string)
        score (atom 0)]
    (loop [i 0
           level 0]
      (when (< i (count input-vec))
        (let [char (get input-vec i)]
          (cond
            (= char \{) (do
                          (swap! score (fn [v l] (+ v (inc l))) level)
                          (recur (inc i) (inc level)))
            (= char \}) (recur (inc i) (dec level))
            (= char \<) (recur (parse-garbage input-vec (inc i)) level)
            :else (recur (inc i) level)))))
    @score))

(defn get-input-data
  []
  (-> (slurp "resources/day09.part1.input")
      (str/trim-newline)))

(defn run
  []
  (traverse (get-input-data)))
