(ns aoc.day07.part1
  (:require [clojure.string :as str]))

(def leaf-pattern #"(\w+)\s\((\d+)\)")
(def node-pattern #"(\w+)\s\((\d+)\)\s->\s(\w+(?:,\s\w+)*)")

(defn parse-leaf
  [matches]
  {:name (get matches 1)
   :weight (Integer/parseInt (get matches 2))})

(defn parse-node
  [matches]
  {:name (get matches 1)
   :weight (Integer/parseInt (get matches 2))
   :children (str/split (get matches 3) #",*\s+")})

(defn parse-line
  [line]
  (if-let [leaf-match (re-matches leaf-pattern line)]
    (parse-leaf leaf-match)
    (if-let [node-match (re-matches node-pattern line)]
      (parse-node node-match)
      nil)))

(defn get-input-data
  []
  (->> (slurp "resources/day07.part1.input")
       (str/split-lines)
       (map parse-line)))
