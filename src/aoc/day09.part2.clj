(ns aoc.day09.part2
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

(def test-input-9 {:input "<>"
                   :garbage 0})

(def test-input-10 {:input "<random characters>"
                    :garbage 17})

(def test-input-11 {:input "<<<<>"
                    :garbage 3})

(def test-input-12 {:input "<{!>}>}"
                    :garbage 2})

(def test-input-13 {:input "<!!>"
                    :garbage 0})

(def test-input-14 {:input "<!!!>>"
                    :garbage 0})

(def test-input-15 {:input "<{o\"i!a,<{i<a>"
                    :garbage 10})

(defn parse-garbage
  [garbage index]
  (let [i (atom index)
        count (atom 0)]
    (loop [closed? false]
      (when (not closed?)
        (let [char (get garbage @i)]
          (cond
            (= char \>) (do (swap! i inc)
                            (recur true))
            (= char \!) (do (swap! i #(+ % 2))
                            (recur false))
            :else (do (swap! i inc)
                      (swap! count inc)
                      (recur false))))))
    {:index @i
     :counted @count}))

(defn add-groups
  [score-map value]
  (update-in score-map [:groups] + value))

(defn add-garbage
  [score-map value]
  (update-in score-map [:garbage] + value))

(defn traverse
  [input-string]
  (let [input-vec (vec input-string)
        score (atom {:groups 0
                     :garbage 0})]
    (loop [i 0
           level 0]
      (when (< i (count input-vec))
        (let [char (get input-vec i)]
          (cond
            (= char \{) (do
                          (swap! score add-groups (inc level))
                          (recur (inc i) (inc level)))
            (= char \}) (recur (inc i) (dec level))
            (= char \<) (do
                          (let [garbage (parse-garbage input-vec (inc i))
                                next (:index garbage)
                                counted (:counted garbage)]
                            (swap! score add-garbage counted)
                            (recur next level)))
            :else (recur (inc i) level)))))
    @score))

(defn get-input-data
  []
  (-> (slurp "resources/day09.part2.input")
      (str/trim-newline)))

(defn run
  []
  (traverse (get-input-data)))
