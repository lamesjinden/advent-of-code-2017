(ns aoc.day08.part1
  (:require [clojure.string :as str]))

(def sample-line "p inc -598 if fk < 3587")

(def line-pattern #"(\w+) (\w+) (-?\d+) (.*)")
(def predicate-pattern #"if (\w+) (==|!=|>|>=|<|<=) (-?\d+)")

(defn parse-predicate
  [predicate]
  (let [matching-groups (re-matches predicate-pattern predicate)]
    (when (nil? matching-groups)
      (throw (Exception. (str "failed to parse predicate: " predicate))))
    {:subject (get matching-groups 1)
     :operator (get matching-groups 2)
     :argument (get matching-groups 3)}))

(defn parse-line
  [line]
  (let [matching-groups (re-matches line-pattern line)]
    (when (nil? matching-groups)
      (throw (Exception. (str "failed to parse: " line))))
    {:register (get matching-groups 1)
     :operation (get matching-groups 2)
     :value (get matching-groups 3)
     :predicate (parse-predicate (matching-groups 4))}))

(defn get-input-data
  []
  (->> (slurp "resources/day08.part1.input")
       (str/split-lines)
       (map parse-line)))

(defn create-environment
  [instructions]
  (->> instructions
       (map (fn [instruction]
              {(get instruction :register)
               (atom 0)}))))

(defn predicate-true?
  [predicate environment]
  true)

(defn apply-instruction
  [instruction environment])

(defn evaluate
  [instructions]
  (let [environment (create-environment instructions)]
    (doseq [instruction instructions]
      (when (predicate-true? (get instruction :predicate) environment)
          apply-instruction))
    (key (apply max-key #(deref (val %)) environment))))

(defn run
  []
  (evaluate (get-input-data)))
