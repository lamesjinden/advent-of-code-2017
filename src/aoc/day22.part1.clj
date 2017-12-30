(ns aoc.day22.part1
  (:require [clojure.string :as str]))

(def test-input (str "..#\n"
                     "#..\n"
                     "...\n"))

(def input-file "resources/day22.part1.input")

(defn string-indexed
  [s]
  (->> s
       (seq)
       (map-indexed vector)
       (into (sorted-map))))

(defn string->grid
  [s]
  (->> s
       (str/split-lines)
       (map-indexed #(vector %1 (string-indexed %2)))
       (into (sorted-map))))

(defn file->grid
  [file-path]
  (->> (slurp file-path)
       (string->grid)))

(def next-direction-infected {:up :right
                              :right :down
                              :down :left
                              :left :up})

(def next-direction-clean {:up :left
                           :left :down
                           :down :right
                           :right :up})

(defn infected?
  [cell]
  (= cell \#))

(defn next-direction
  [cell direction]
  (if (infected? cell)
    (direction next-direction-infected)
    (direction next-direction-clean)))

(defn start-position
  [grid]
  {:x (-> grid
          (first)
          (get 1)
          (count)
          (/ 2)
          (Math/floor)
          (int))
   :y (-> grid
          (count)
          (/ 2)
          (Math/floor)
          (int))})

(def direction-transitions {:up {:x -1 :y 0}
                            :down {:x 1 :y 0}
                            :left {:x 0 :y -1}
                            :right {:x 0 :y 1}})

(defn next-position
  [x y direction]
  (let [transition (get direction-transitions direction)]
    {:x (+ x (:x transition))
     :y (+ y (:y transition))}))

(def cell-transitions {\. \#
                       \# \.
                       nil \#})

(defn next-cell-state
  [grid x y]
  (update-in grid [x y] #(get cell-transitions %)))

(defn did-infect?
  [grid grid' x y]
  (and (not= \# (get-in grid [x y]))
       (= \# (get-in grid' [x y]))))

(defn burst-step
  [{:keys [grid direction infections] {:keys [x y] :as position} :position :as step}]
  (let [next-direction (next-direction (get-in grid [x y]) direction)
        next-grid (next-cell-state grid x y)
        next-position (next-position x y next-direction)
        next-infections (if (did-infect? grid next-grid x y) (inc infections) infections)]
    {:direction next-direction
     :grid next-grid
     :position next-position
     :infections next-infections}))

(defn create-world
  ([initial-grid]
   (let [grid initial-grid
         position (start-position grid)
         direction :up]
     {:grid grid
      :position position
      :direction direction
      :infections 0})))

(defn run
  ([] (run 10000))
  ([times] (run times (create-world (file->grid input-file))))
  ([times world]
   (reduce (fn [state i] (burst-step state)) world (range times))))
