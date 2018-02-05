(ns aoc.day12.part1
  (:require [clojure.string :as str]))

(def test-input (str "0 <-> 2\n"
                     "1 <-> 1\n"
                     "2 <-> 0, 3, 4\n"
                     "3 <-> 2, 4\n"
                     "4 <-> 2, 3, 6\n"
                     "5 <-> 6\n"
                     "6 <-> 4, 5\n"))

(def line-pattern #"(\d+) <-> (\d+(, \d+)*)")

(defn parse-line
  [line]
  (let [matches (re-matches line-pattern line)
        node (get matches 1)
        nodes (str/split (get matches 2) #",\s*")]
    {:node  node
     :nodes nodes}))

(defn join-nodes
  [{:keys [node nodes]}]
  (map conj (repeat [node]) nodes))

(defn build-graph
  [state [node1 node2]]
  (update-in state [node1] (fn [v] (clojure.set/union v #{node2}))))

(defn string->graph
  [s]
  (->> s
       (str/split-lines)
       (map parse-line)
       (mapcat join-nodes)
       (reduce build-graph {})))

(defn file->graph
  [path]
  (-> (slurp path)
      (string->graph)))

(def input-file-path "resources/day12.part1.input")

(def test-graph (string->graph test-input))
(def input-graph (file->graph input-file-path))

(defn traverse
  ([graph start-vertex]
   (traverse graph [] #{} [start-vertex]))
  ([graph vertices visited frontier]
   (if (empty? frontier)
     vertices
     (let [vertex (peek frontier)
           neighbors (get graph vertex)]
       (if (contains? visited vertex)
         (recur graph
                vertices
                visited
                (pop frontier))
         (recur graph
                (conj vertices vertex)
                (conj visited vertex)
                (into (pop frontier)
                      (filter #(not (contains? visited %)) neighbors))))))))

(defn run
  []
  (count (traverse input-graph "0")))
