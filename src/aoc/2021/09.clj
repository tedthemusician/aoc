(ns aoc.2021.09
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [[2 1 9 9 9 4 3 2 1 0]
             [3 9 8 7 8 9 4 9 2 1]
             [9 8 5 6 7 8 9 8 9 2]
             [8 7 6 7 8 9 6 7 8 9]
             [9 8 9 9 9 6 5 6 7 8]])

(def offsets {:up [0 -1]
              :right [1 0]
              :down [0 1]
              :left [-1 0]})

(defn parse-line
  [line]
  (mapv (comp edn/read-string str) (vec line)))

(defn parse-lines
  [lines]
  (mapv parse-line lines))

(defn local-min
  "If y is less than x and less than z, return it; otherwise return nil."
  [[x y z]]
  (if (and (< y x) (< y z))
    y
    nil))

(defn value-at
  "The value of (`x`, `y`) in matrix `rows`"
  [rows x y]
  (nth (nth rows y) x))

(defn get-neighbor
  "Get the neighbor of (`x`, `y`) in direction `dir` of matrix `rows`. If the
  neighbor in that direction is past the edge, return nil."
  [rows x y dir]
  (let [[dx dy] (get offsets dir)
        x' (+ x dx)
        y' (+ y dy)]
    (if (or (< x' 0)
            (>= x' (count (first rows)))
            (< y' 0)
            (>= y' (count rows)))
      nil
      {:x x'
       :y y'
       :value (value-at rows x' y')})))

(defn get-neighbors
  "Get all cardinal neighbors of (`x`, `y`) in matrix `rows`."
  [rows x y]
  (keep (partial get-neighbor rows x y) [:up :right :down :left]))

(defn local-min
  "Return the coordinates and value of matrix `rows` at (`x`, `y`) if it is less
  than all of its neighbors; otherwise return nil."
  [rows x y]
  (let [value (value-at rows x y)
        neighbor-vals (map :value (get-neighbors rows x y))]
    (if (every? #(< value %) neighbor-vals)
      {:x x
       :y y
       :value value}
      nil)))

(defn get-local-mins
  "Get every local minimum in rows, i.e. every value that is less than all of
  its neighbors."
  [rows]
  (let [width (count (first rows))
        height (count rows)
        coords (for [x (range 0 width) y (range 0 height)] [x y])]
    (keep #(local-min rows (first %) (second %)) coords)))

(defn higher?
  "Return whether the value of `point` is less than 9 and greater than `value`."
  [value point]
  (let [point-value (:value point)]
    (and (< point-value 9) (> point-value value))))

(defn get-higher-neighbors
  "Get every point around (`x`, `y`) in matrix `rows` that is higher than that
  point. Exclude nines and points whose coordinates are outisde the matrix."
  [rows x y]
  (let [value (value-at rows x y)
        neighbors (get-neighbors rows x y)]
    (filter (partial higher? value) neighbors)))

(defn extend-known-basin
  "Add the neighbors of the outer points of a basin to the basin if those
  neighbors are higher than the given outer point."
  [rows {:keys [inner outer] :as basin}]
  (let [higher-neighbors
        (distinct (mapcat #(get-higher-neighbors rows (:x %) (:y %)) outer))]
    {:inner (distinct (concat outer inner))
     :outer higher-neighbors}))

(defn get-basin
  "All points in `rows` that flow down toward a single local minimum at
  (`x`, `y`)"
  [rows x y]
  (let [lowest-point {:x x :y y :value (value-at rows x y)}
        initial-basin {:outer [lowest-point] :inner []}
        basin-stages (iterate (partial extend-known-basin rows) initial-basin)
        ]
    (:inner (first (drop-while #(not-empty (:outer %)) basin-stages)))))

(defn solve-1
  [rows]
  (reduce + (map (comp inc :value) (get-local-mins rows))))

(defn solve-2
  [rows]
  (let [min-coords (map (juxt :x :y) (get-local-mins rows))
        basins (map (partial apply get-basin rows) min-coords)
        largest-3-basin-sizes (take 3 (sort > (map count basins)))]
    (reduce * largest-3-basin-sizes)))

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 15 :input 633}
   {:method solve-2 :sample 1134 :input 1050192}]
  {:value sample}
  (parse-lines (utils/get-lines 2021 9)))
