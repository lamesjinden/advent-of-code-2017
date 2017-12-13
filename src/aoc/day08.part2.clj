(ns aoc.day08.part2
  (:require [clojure.string :as str]))

(def line-pattern #"(\w+) (\w+) (-?\d+) (.*)")
(def predicate-pattern #"if (\w+) (==|!=|>|>=|<|<=) (-?\d+)")

(def predicates {"==" (fn [atom arg] (= @atom arg))
                 "!=" (fn [atom arg] (not (= @atom arg)))
                 ">"  (fn [atom arg] (> @atom arg))
                 ">=" (fn [atom arg] (>= @atom arg))
                 "<"  (fn [atom arg] (< @atom arg))
                 "<=" (fn [atom arg] (<= @atom arg))})

(def operations {"inc" (fn [atom arg] (swap! atom #(+ % arg)))
                 "dec" (fn [atom arg] (swap! atom #(- % arg)))})

(defn parse-predicate
  [predicate]
  (let [matching-groups (re-matches predicate-pattern predicate)]
    (when (nil? matching-groups)
      (throw (Exception. (str "failed to parse predicate: " predicate))))
    {:subject (get matching-groups 1)
     :operator (get matching-groups 2)
     :argument (Integer/parseInt (get matching-groups 3))}))

(defn parse-line
  [line]
  (let [matching-groups (re-matches line-pattern line)]
    (when (nil? matching-groups)
      (throw (Exception. (str "failed to parse: " line))))
    {:register (get matching-groups 1)
     :operation (get matching-groups 2)
     :value (Integer/parseInt (get matching-groups 3))
     :predicate (parse-predicate (matching-groups 4))}))

(defn get-input-data
  []
  (->> (slurp "resources/day08.part2.input")
       (str/split-lines)
       (map parse-line)))

(defn create-environment
  [instructions]
  (->> instructions
       (map (fn [instruction]
              {(get instruction :register)
               (atom 0)}))
       (into {:max (atom 0)})))

(defn predicate-true?
  [predicate environment]
  (let [subject-name (get predicate :subject)
        subject-value (get environment subject-name)
        operator-name (get predicate :operator)
        operator-fn (get predicates operator-name)
        argument-value (get predicate :argument)]
    (operator-fn subject-value argument-value)))

(defn apply-instruction
  [instruction environment]
  (let [register-name (get instruction :register)
        operation-name (get instruction :operation)
        operation-value (get instruction :value)
        operation-fn (get operations operation-name)]
    (operation-fn (get environment register-name) operation-value)
    (let [operation-result-value (deref (get environment register-name))
          current-max-value (deref (get environment :max))]
      (when (> operation-result-value current-max-value)
      (reset! (get environment :max) operation-result-value)))))

(defn evaluate
  [instructions]
  (let [environment (create-environment instructions)]
    (doseq [instruction instructions]
      (when (predicate-true? (get instruction :predicate) environment)
        (apply-instruction instruction environment)))
    environment))

(defn run
  []
  (deref (get (evaluate (get-input-data)) :max)))

