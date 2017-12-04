(ns aoc.day01.part2
  (:require [clojure.pprint :as pp]))

(def test-data-1 { :input [1 2 1 2] :output 6 })

(def test-data-2 { :input [1 2 2 1] :output 0 })

(def test-data-3 { :input [1 2 3 4 2 5] :output 4 })

(def test-data-4 { :input [1 2 3 1 2 3] :output 12 })

(def test-data-5 { :input [1 2 1 3 1 4 1 5] :output 4})

(def tests
  [test-data-1
   test-data-2
   test-data-3
   test-data-4
   test-data-5])

(defn reverse-captcha
  [numbers]
  (if (empty? numbers)
    0
    (let [num-vec (vec numbers)
          count (count num-vec)
          step (/ count 2)]
      (:result
       (reduce-kv (fn [state index element]
                    (let [complement (mod (+ index step) count)]
                      (if (= element (get num-vec complement))
                        {:result (+ element (:result state))}
                        state)))
                  {:result 0} num-vec)))))

(defn run-test
  [test-datum]
  (assoc test-datum
         :result (reverse-captcha (:input test-datum))))

(defn run-tests
  []
  (->> tests
       (map run-test)
       (map
        #(assoc %
                :test-result
                (if (= (:output %) (:result %))
                  "success"
                  "failed")))))

(defn get-input-data
  []
  (->> (slurp "resources/day01.part2.input")
       (seq) ;;string to seq of chars
       (filter #(Character/isDigit %)) ;;strip newline
       (map str) ;;seq of chars to seq of strings
       (map #(Integer/parseInt %)))) ;;seq of strings to seq of integers

(defn run
  []
  (reverse-captcha (get-input-data)))
