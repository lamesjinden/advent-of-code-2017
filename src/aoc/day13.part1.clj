(ns aoc.day13.part1
  (:require [clojure.string :as str]))

(def line-pattern #"^(\d+):\s(\d+)\s*$")

(defn create-layer
  []
  {:index 0
   :range 0
   :scanner-direction :down
   :scanner-position 0})

(defn line->data
  [line]
  (let [match (re-matches line-pattern line)]
    (-> (create-layer)
        (update :index (fn [_] (Integer/parseInt (get match 1))))
        (update :range (fn [_] (Integer/parseInt (get match 2)))))))

(defn create-firewall
  []
  {:layers []
   :layers-caught []
   :packet-index -1})

(defn string->firewall
  [input-string]
  (let [firewall (create-firewall)
        layers (->> input-string
                    (str/split-lines)
                    (map line->data)
                    (into []))]
    (update firewall :layers (fn [_] layers))))

(defn file->firewall
  [file-path]
  (-> file-path
      (slurp)
      (string->firewall)))

(defn index-by-layer
  [layers]
  (->> layers
       (map (fn [x] [(:index x) x]))
       (into {})))

(defn print-firewall-header
  [min-layer max-layer]
  (loop [i min-layer]
    (when (<= i max-layer)
      (printf " %s  " i)
      (recur (inc i))))
  (newline))

(defn get-layers-bounds
  [{:keys [layers]}]
  {:min-layer (:index (apply min-key :index layers))
   :max-layer (:index (apply max-key :index layers))
   :max-range (:range (apply max-key :range layers))})

(defn print-firewall-line
  [{:keys [layers] :as firewall} line-index]
  (let [{:keys [min-layer max-layer]} (get-layers-bounds firewall)
        by-layer (index-by-layer layers)]
    (loop [layer-index min-layer]
      (when (<= layer-index max-layer)
        (if (and (= line-index min-layer) (not (contains? by-layer layer-index)))
          (if (= (:packet-index firewall) layer-index)
            (print "(.) ")
            (print "... "))
          (if (not (< line-index (get-in by-layer [layer-index :range] min-layer)))
            (print "    ")
            (cond
              (and
                (= 0 line-index (:scanner-position (get by-layer layer-index)))
                (= (:packet-index firewall) layer-index))
              (print "(S) ")
              (and
                (= 0 line-index)
                (= (:packet-index firewall) layer-index))
              (print "( ) ")
              (= line-index
                 (:scanner-position (get by-layer layer-index)))
              (print "[S] ")
              :else
              (print "[ ] "))))
        (recur (inc layer-index))))
    (newline)))

(defn print-firewall
  [{:keys [layers] :as firewall}]
  (let [{:keys [min-layer max-layer max-range]} (get-layers-bounds firewall)]
    (print-firewall-header min-layer max-layer)
    (loop [i 0]
      (when (< i max-range)
        (print-firewall-line firewall i)
        (recur (inc i)))))
  firewall)

(defn move-scanner
  [layer]
  (case (:scanner-direction layer)
    :up (if (= 0 (:scanner-position layer))
          (-> layer
              (update :scanner-position inc)
              (update :scanner-direction (fn [x] :down)))
          (-> layer
              (update :scanner-position dec)))
    :down (if (= (dec (:range layer)) (:scanner-position layer))
            (-> layer
                (update :scanner-position dec)
                (update :scanner-direction (fn [x] :up)))
            (-> layer
                (update :scanner-position inc)))))

(defn packet-caught?
  [{:keys [layers packet-index] :as firewall}]
  (let [by-layer (index-by-layer layers)
        packet-layer (get by-layer packet-index)]
    (= 0 (:scanner-position packet-layer))))

(defn firewall-tick
  [firewall]
  (as-> firewall $
        (update $ :packet-index inc)
        (if (packet-caught? $)
          (update $ :layers-caught (fn [x] (conj x (:packet-index $))))
          $)
        (update $ :layers (fn [x] (mapv move-scanner x)))))

(defn get-layer-score
  [{:keys [range index] :as layer}]
  (* range index))

(defn get-severity
  [{:keys [layers layers-caught] :as firewall}]
  (let [by-layer (index-by-layer layers)]
    (reduce #(+ %1 (get-layer-score (by-layer %2))) 0 layers-caught)))

(def test-input (str "0: 3\n"
                     "1: 2\n"
                     "4: 4\n"
                     "6: 4\n"))

(def test-firewall (string->firewall test-input))

(def input-file "resources/day13.part1.input")

(defn run
  ([] (run (file->firewall input-file)))
  ([{:keys [layers] :as firewall}]
   (let [{:keys [min-layer max-layer]} (get-layers-bounds firewall)
         end-state (reduce
                     (fn [acc i] (firewall-tick acc))
                     firewall
                     (range min-layer (inc max-layer)))]
     (get-severity end-state))))
