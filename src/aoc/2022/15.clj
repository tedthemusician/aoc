(ns aoc.2022.15
  (:require [aoc.utils :as utils]
            [clojure.edn :as edn])
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

(defn get-distance
  "Get the Manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (utils/abs (- x1 x2)) (utils/abs (- y1 y2))))

(defn assoc-radius
  "Assoc an input line's radius with its sensor and beacon points"
  [{:keys [sensor beacon] :as points}]
  (assoc points :radius (get-distance sensor beacon)))

(def input-re #"^Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)$")

(defn parse-line
  "Get the sesnor point and closest-beacon point from a line"
  [line]
  (let [[sx sy bx by] (map edn/read-string (rest (re-matches input-re line)))]
    (assoc-radius {:sensor [sx sy], :beacon [bx, by]})))

(defn get-row-at
  "Given the position of a sensor, the distance to its nearest beacon, and a
  y-value, return the x bounds of that sensor's coverage"
  [row-y {:keys [sensor radius]}]
  (let [[sensor-x sensor-y] sensor
        dy (utils/abs (- sensor-y row-y))
        distance (- radius dy)]
    (if (> dy radius)
      nil
      [(- sensor-x distance) (+ sensor-x distance)])))

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
  [ranges]
  (let [[r & rs] (sort (remove nil? ranges))]
    (loop [merged (list r), remaining rs]
      (if (empty? remaining)
        (reverse merged)
        (let [r1 (first merged)
              [r2 & remaining'] remaining
              merged' (if (contiguous-or-overlapping? r1 r2)
                        (cons (merge-two-ranges r1 r2) (rest merged))
                        (cons r2 merged))]
          (recur merged' remaining'))))))

(defn get-ranges-at-row
  "Get all sensors' x ranges at a given a row"
  [row sensors]
  (merge-all-ranges (map (partial get-row-at row) sensors)))

(defn range-in-bounds?
  "Does a range overlap with the region between 0 and hi?"
  [hi x-range]
  (and (some #(>= % 0) x-range)
       (some #(<= % hi) x-range)))

(defn keep-in-bounds
  "Keep only ranges that overlap with the region between 0 and hi"
  [hi ranges]
  (filter (partial range-in-bounds? hi) ranges))

(defn solve-1
  [{:keys [row input]}]
  (->> input
       (map parse-line)
       (get-ranges-at-row row)
       (map #(utils/abs (- (second %) (first %))))
       (reduce +)))

(defn solve-2
  [{:keys [maximum input]}]
  (let [sensors (map parse-line input)]
    (loop [y 0]
      (if (> y maximum)
        "FAILURE"
        (let [ranges (get-ranges-at-row y sensors)
              ranges-in-bounds (keep-in-bounds maximum ranges)]
          (if (> (count ranges-in-bounds) 1)
            (let [[r1 r2] ranges-in-bounds
                  x (inc (second r1))]
              (+ y (* 4000000 x)))
            (recur (inc y))))))))

(utils/verify-solutions
  [{:method solve-1 :sample 26 :input 5716881}
   {:method solve-2 :sample 56000011 :input 10852583132904}]
  {:value {:row 10 :maximum 20 :input sample}}
  {:row 2000000 :maximum 4000000 :input (utils/get-lines 2022 15)})
