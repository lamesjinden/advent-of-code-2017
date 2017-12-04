(ns aoc.day01.part1
  (:require [clojure.pprint :as pp]))

(def test-data-1 { :input [1 1 2 2] :output 3 })

(def test-data-2 { :input [1 1 1 1] :output 4 })

(def test-data-3 { :input [1 2 3 4] :ouptut 0 })

(def test-data-4 { :input [9 1 2 1 2 1 2 9] :output 9 })

(def tests
  [test-data-1
   test-data-2
   test-data-3
   test-data-4])

(defn reverse-captcha
  [numbers]
  (if (empty? numbers)
    0
    (let [numbers-normalized (concat numbers [(first numbers)])]
      (:result
       (reduce (fn [m v]
                 (if (nil? m)
                   {:prev v
                    :result 0}
                   (if (not (= v (:prev m)))
                     {:prev v
                      :result (:result m)}
                     {:prev v
                      :result (+ v (:result m))}
                     )))
               nil numbers-normalized)))))

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
  (->> (slurp "resources/day01.part1.input")
       (seq) ;;string to seq of chars
       (filter #(Character/isDigit %)) ;;strip newline
       (map str) ;;seq of chars to seq of strings
       (map #(Integer/parseInt %)))) ;;seq of strings to seq of integers

(defn run
  []
  (reverse-captcha (get-input-data)))
