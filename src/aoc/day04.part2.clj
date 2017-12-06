(ns aoc.day04.part2
  (:require [clojure.string :as str]))

(defn get-input-data
  []
  (->> (slurp "resources/day04.part2.input")
       (str/split-lines)
       (map #(str/split % #"\s+"))))

(defn valid?
  [passphrase]
  (let [token-count (count passphrase)
        unique-count (count (apply hash-set passphrase))
        valid (= token-count unique-count)]
    valid))

(defn validate-passwords
  [passphrases]
  (count
   (filter valid? passphrases)))
    
(defn run
  []
  (validate-passwords (get-input-data)))
