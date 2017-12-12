(ns aoc.day07.part2
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

(defn get-input-data
  []
  (->> (slurp "resources/day07.part2.input")
       (str/split-lines)
       (map parse-line)
       (reduce (fn [state item] (assoc state (:name item) item)) {})))

(defn find-root
  [nodes]
  (let [names (set (map :name nodes))
        children (set (mapcat :children nodes))
        root (set/difference names children)]
    (first root)))

(defn leaf?
  [node]
  (nil? (:children node)))

(defn traverse
  [node nodes]
  (if (leaf? node)
    (println (:name node) "(leaf)")
    (do
      (doseq [child (:children node)]
        (traverse (get nodes child) nodes))
      (println (:name node) "(node)"))))

(defn get-weight
  [node nodes]
  (if (leaf? node)
    (:weight node)
    (+ (:weight node)
       (reduce (fn [value node-name]
                 (+ value (get-weight (nodes node-name) nodes)))
               0
               (:children node)))))

(defn balanced?
  [node nodes]
  (if (leaf? node)
    true
    (if (= 1 (count (set (map #(get-weight (nodes %) nodes) (:children node)))))
      true
      false)))

(defn run
  []
  (find-root (get-input-data)))
