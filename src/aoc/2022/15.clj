(ns aoc.2022.15
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set])
  (:gen-class))

(def sample ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
             "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
             "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
             "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
             "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
             "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
             "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
             "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
             "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
             "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
             "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
             "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
             "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
             "Sensor at x=20, y=1: closest beacon is at x=15, y=3"])

(defn parse-line
  "Get the sesnor point and closest-beacon point from a line"
  [line]
  (let [[sx sy bx by] (map edn/read-string
                           (rest (re-matches #"^Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)$"
                                             line)))]
    {:sensor [sx sy], :beacon [bx, by]}))

(defn parse-input
  "Given a map with keys :row and :input, update :input by mapping parse-line"
  [input]
  (update input :input (partial map parse-line)))

(def i (:input (parse-input {:row 10 :input sample})))

(defn get-distance
  "Get the Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (utils/abs (- x1 x2)) (utils/abs (- y1 y2))))

(defn get-row-at
  "Given the position of a sensor, the distance to its nearest beacon, and a
  y-value, return the x bounds of that sensor's coverage"
  [[point-x point-y] distance row-y]
  (let [dy (utils/abs (- point-y row-y))
        radius (- distance dy)]
    [(- point-x radius) (+ point-x radius)]))

(defn get-ranges-by-row
  "Given a map of a sensor and its nearest beacon, return a mapping from
  y-value to x bounds"
  [{:keys [sensor beacon]}]
  (let [[sensor-x sensor-y] sensor
        distance (get-distance sensor beacon)
        ys (range (- sensor-y distance) (inc (+ sensor-y distance)))]
    (zipmap ys (map (partial get-row-at sensor distance) ys))))

(defn contiguous-or-overlapping?
  "Are two ranges either contiguous or overlapping?"
  [[s1 e1] [s2 e2]]
  (or (and (>= s2 s1) (<= s2 (inc e1)))
      (and (>= s1 s2) (<= s1 (inc e2)))))

(defn merge-two-ranges
  "Given two ranges, create a new range from the lower start to the higher end"
  [[s1 e1] [s2 e2]]
  [(min s1 s2) (max e1 e2)])

(defn merge-all-ranges
  "Given a list of ranges, merge all that overlap"
  [[r & rs]]
  (loop [merged (list r), remaining rs]
    (if (empty? remaining)
      (reverse merged)
      (let [r1 (first merged)
            [r2 & remaining'] remaining
            merged' (if (contiguous-or-overlapping? r1 r2)
                      (cons (merge-two-ranges r1 r2) (rest merged))
                      (cons r2 merged))]
        (recur merged' remaining')))))

(defn solve-1-fast
  [{:keys [row input]}]
  (->> input
       (map (comp (fn [{:keys [sensor beacon]}]
                    (get-row-at sensor (get-distance sensor beacon) row))
                  parse-line))
       sort
       merge-all-ranges
       (map #(utils/abs (- (second %) (first %))))
       (reduce +)))

(defn solve-1-complete
  [{:keys [row input]}]
  (->> input
     (map (comp #(get % row) get-ranges-by-row parse-line))
     (remove nil?)
     sort
     merge-all-ranges
     (map #(utils/abs (- (second %) (first %))))
     (reduce +)))

(utils/verify-solutions
  [{:method solve-1-fast :sample 26 :input 5716881}
   #_ {:method solve-2 :sample 56000011 :input nil}]
  {:value {:row 10 :input sample}}
  {:row 2000000 :input (utils/get-lines 2022 15)})
