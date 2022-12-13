(ns aoc.2022.12
  (:require [aoc.utils :as utils]
            [clojure.set :as set])
  (:gen-class))

(def sample ["Sabqponm"
             "abcryxxl"
             "accszExk"
             "acctuvwj"
             "abdefghi"])

(defn parse-char
  "Parse S as :start, E as :end, or any other lowercase letter as its distance
  past lowercase a"
  [c]
  (case c
    \S :start
    \E :end
    (- (int c) (int \a))))

(defn find-point
  "Find a point in a matrix whose value is label"
  [m label]
  (let [y (utils/find-index #((set %) label) m)
        x (utils/find-index (partial = label) (nth m y))]
    [x y]))

(defn parse-input
  "Parse input as a matrix of heights, a start point, and an end point"
  [input]
  (let [matrix (mapv (partial mapv parse-char) input)
        start (find-point matrix :start)
        end (find-point matrix :end)]
    {:matrix (mapv (fn [row]
                     (mapv (fn [cell]
                             (case cell
                               :start 0
                               :end 25
                               cell))
                           row))
                   matrix)
     :start start
     :end end}))

(def s (parse-input sample))
(def mat (:matrix s))

(def adjustments [[-1 0] [0 -1] [1 0] [0 1]])

(def add-points (partial mapv +))

(def width (comp count first))

(def height count)

(defn get-points [m]
  "Get all coordinates of a matrix"
  (for [x (range (width m)) y (range (height m))] [x y]))

(defn get-at
  "Get the value of the cell of a matrix at a coordinate"
  [m [x y]]
  (get-in m [y x]))

(defn inbounds?
  "Is a coordinate within the bounds of a matrix?"
  [m [x y]]
  (and (>= x 0)
       (>= y 0)
       (< x (count (first m)))
       (< y (count m))))

(defn get-neighbor-points
  "Get all points of a matrix that neighbor a given point"
  [m point]
  (let [neighbors (map (partial add-points point) adjustments)]
    (filter (partial inbounds? m) neighbors)))

(defn get-height-difference
  "Get the height difference, i.e. the difference in value, between two points
  in a matrix"
  [m c1 c2]
  (let [h1 (get-at m c1)
        h2 (get-at m c2)]
    (- h2 h1)))

(defn get-reachable-neighbors
  "Get the neighbors of a given point that differ according to reachability-pred"
  [reachability-pred m point]
  (let [level (get-at m point)
        neighbors (get-neighbor-points m point)]
    (set (filter #(reachability-pred (get-height-difference m point %)) neighbors))))

(defn initialize-connections
  "Create a mapping of points to reachable neighbors"
  [reachability-pred m]
  (let [points (get-points m)
        neighbors (map (partial get-reachable-neighbors reachability-pred m) points)]
    (zipmap points neighbors)))

(defn explore-farthest
  "Find which unreached points can be reached by the farthest known points"
  [connections distance-groups]
  (let [known-points (apply set/union distance-groups)
        active-points (last distance-groups)
        reachable-points (set (mapcat (partial get connections) active-points))
        new-points (set/difference reachable-points known-points)]
    (conj distance-groups new-points)))

(defn explore-all
  "Explore a matrix via its connections along all paths"
  [connections origin]
  (->> [#{origin}]
       (iterate (partial explore-farthest connections))
       (map last)
       (take-while (partial seq))))

(defn solve-1
  [input]
  (let [{:keys [matrix start end]} (parse-input input)
        reachability-pred #(<= % 1)
        connections (initialize-connections reachability-pred matrix)
        distance-groups (explore-all connections start)]
    (utils/find-index #(% end) distance-groups)))

(defn solve-2
  [input]
  (let [{:keys [matrix start end]} (parse-input input)
        reachability-pred #(>= % -1)
        connections (initialize-connections reachability-pred matrix)
        distance-groups (explore-all connections end)]
    (utils/find-index (fn [points]
                        (some #(zero? (get-at matrix %)) points))
                      distance-groups)))

(utils/verify-solutions
  [{:method solve-1 :sample 31 :input 468}
   {:method solve-2 :sample 29 :input 459}]
  {:value sample}
  (utils/get-lines 2022 12))
