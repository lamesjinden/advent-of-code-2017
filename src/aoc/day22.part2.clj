(ns aoc.day22.part2
  (:require [clojure.string :as str]))

(def next-direction-clean {:up    :left
                           :left  :down
                           :down  :right
                           :right :up})

(def next-direction-weakened {:up    :up
                              :down  :down
                              :left  :left
                              :right :right})

(def next-direction-infected {:up    :right
                              :right :down
                              :down  :left
                              :left  :up})

(def next-direction-flagged {:up    :down
                             :down  :up
                             :left  :right
                             :right :left})

(def next-direction-table {nil next-direction-clean
                           \.  next-direction-clean
                           \W  next-direction-weakened
                           \#  next-direction-infected
                           \F  next-direction-flagged})

(defn next-direction
  [cell direction]
  (get-in next-direction-table [cell direction]))

(def direction-transitions {:up    {:x -1 :y 0}
                            :down  {:x 1 :y 0}
                            :left  {:x 0 :y -1}
                            :right {:x 0 :y 1}})

(defn next-position
  [x y direction]
  (let [transition (get direction-transitions direction)
        x' (+ x (:x transition))
        y' (+ y (:y transition))]
    {:x x'
     :y y'}))

(def cell-transitions {nil \W
                       \.  \W
                       \W  \#
                       \#  \F
                       \F  \.})

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
    {:direction  next-direction
     :grid       next-grid
     :position   next-position
     :infections next-infections}))

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

(defn create-world
  [initial-grid]
  (let [grid initial-grid
        position (start-position grid)
        direction :up]
    {:grid       grid
     :position   position
     :direction  direction
     :infections 0}))

(def test-input (str "..#\n"
                     "#..\n"
                     "...\n"))

(def input-file "resources/day22.part2.input")

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

(defn run
  ([]
   (run 10000000))
  ([times]
   (run times (create-world (file->grid input-file))))
  ([times world]
   (reduce (fn [state _] (burst-step state)) world (range times))))
