(ns aoc.day07.part1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn find-root
  [nodes]
  (let [names (set (map :name nodes))
        children (set (mapcat :children nodes))
        root (set/difference names children)]
    (first root)))

(defn get-input-data
  []
  (->> (slurp "resources/day07.part1.input")
       (str/split-lines)
       (map parse-line)))

(defn run
  []
  (find-root (get-input-data)))
