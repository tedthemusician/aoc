(ns aoc.2021.05
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["0,9 -> 5,9"
             "8,0 -> 0,8"
             "9,4 -> 3,4"
             "2,2 -> 2,1"
             "7,0 -> 7,4"
             "6,4 -> 2,0"
             "0,9 -> 2,9"
             "3,4 -> 1,4"
             "0,0 -> 8,8"
             "5,5 -> 8,2"])

(defn steps
  "Return a list of integers from start to (and including) stop, descending if
  stop is less than start"
  [start stop]
  (if (< start stop)
    (range start (inc stop))
    (reverse (range stop (inc start)))))

(defn read-coord
  "Get the x and y coords from a string 'x,y'"
  [s]
  (let [[x y] (map edn/read-string (str/split s #","))]
    {:x x :y y}))

(defn read-line-segment
  "Get the starting and ending coords from a string 'x1,y1 -> x2,y2'"
  [s]
  (let [[p1 p2] (map read-coord (str/split s #" -> "))]
    {:x1 (:x p1)
     :y1 (:y p1)
     :x2 (:x p2)
     :y2 (:y p2)}))

(defn straight?
  "Does a line start and end on the same x coord or y coord?"
  [line]
  (let [{:keys [x1 y1 x2 y2]} line]
    (or (= x1 x2)
        (= y1 y2))))

(defn get-points
  "All the points in a line segment"
  [line]
  (let [{:keys [x1 y1 x2 y2]} line
        xs (if (= x1 x2) (repeat x1) (steps x1 x2))
        ys (if (= y1 y2) (repeat y1) (steps y1 y2))]
    (map vector xs ys)))

(defn solve-1
  [lines]
  (let [lines (map read-line-segment lines)
        straight-lines (filter straight? lines)
        points (apply concat (map get-points straight-lines))
        point-counts (frequencies points)]
    (count (filter #(> % 1) (vals point-counts)))))

(defn solve-2
  [lines]
  (let [lines (map read-line-segment lines)
        points (apply concat (map get-points lines))
        point-counts (frequencies points)]
    (count (filter #(> % 1) (vals point-counts)))))

(utils/verify-solutions
  [{:method solve-1 :sample 5 :input 5576}
   {:method solve-2 :sample 12 :input 18144}]
  {:value sample}
  (utils/get-lines 2021 5))
