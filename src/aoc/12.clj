(ns aoc.12
  (:require [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["F10"
             "N3"
             "F7"
             "R90"
             "F11"])

(defn manhattan-distance
  "Get the distance from the origin along the x and y axis"
  [x y]
  (+ (Math/abs x) (Math/abs y)))

(defn parse-move
  "Parse a move"
  [s]
  (let [[_ prefix n-str] (re-matches #"^(\w)(\d+)$" s)
        c (first prefix)
        n (edn/read-string n-str)]
    {:action c :arg n}))

(def headings {\E 0
               \N 90
               \W 180
               \S 270})

(defn move
  "Translate our location by heading and distance along the x or y axis"
  [pos heading distance]
  (let [{:keys [x y]} pos]
    (case heading
      0   (assoc pos :x (+ x distance))
      90  (assoc pos :y (+ y distance))
      180 (assoc pos :x (- x distance))
      270 (assoc pos :y (- y distance)))))

(defn turn-left
  "Update :heading of [pos] by [theta] degrees counterclockwise"
  [pos theta]
  (update-in pos [:heading] (fn [h] (mod (+ h theta) 360))))

(defn turn-right
  "Update :heading [pos] by [theta] degrees clockwise"
  [pos theta]
  (turn-left pos (- 360 theta)))

(defn iter-1
  "Update the ship's position according to rule set 1"
  [pos s]
  (let [{:keys [action arg]} (parse-move s)]
    (case action
      \L (turn-left pos arg)
      \R (turn-right pos arg)
      \F (move pos (:heading pos) arg)
      (move pos (headings action) arg))))

(def travel-1 (partial reduce iter-1 {:heading 0, :x 0, :y 0}))

(defn rotate-left-once
  "Rotate [pos] 90 degrees counterclockwise"
  [{:keys [x y]}]
  {:x (- y), :y x})

(defn rotate-left
  "Rotate [pos] [theta] degrees counterclockwise"
  [pos theta]
  (let [num-rotations (/ theta 90)]
    (first (drop num-rotations (iterate rotate-left-once pos)))))

(defn rotate-right
  "Rotate [pos] [theta] degrees clockwise"
  [pos theta]
  (rotate-left pos (- 360 theta)))

(defn update-waypoint
  "Update the waypoint within a ship's position map"
  [pos f]
  (update-in pos [:waypoint] f))

(defn follow-waypoint
  "Move the ship in the direction of the waypoint [n] times"
  [pos n]
  (let [{:keys [ship waypoint]} pos
        dx (* n (:x waypoint))
        dy (* n (:y waypoint))
        x (+ (:x ship) dx)
        y (+ (:y ship) dy)]
    (assoc pos :ship {:x x :y y})))

(defn iter-2
  "Update the ship or the waypoint's position according to rule set 2"
  [pos s]
  (let [{:keys [action arg]} (parse-move s)]
    (case action
      \L (update-waypoint pos #(rotate-left % arg))
      \R (update-waypoint pos #(rotate-right % arg))
      \F (follow-waypoint pos arg)
      (update-waypoint pos #(move % (headings action) arg)))))

(def travel-2
  (partial reduce iter-2 {:ship {:x 0 :y 0} :waypoint {:x 10 :y 1}}))

(defn solve-1
  "Get the Manhattan distance that results from a sequence of moves according
  to ruleset 1"
  [lines]
  (let [{:keys [x y]} (travel-1 lines)]
    (manhattan-distance x y)))

(defn solve-2
  "Get the Manhattan distance that results from a sequence of moves according
  to ruleset 2"
  [lines]
  (let [{:keys [ship]} (travel-2 lines)
        {:keys [x y]} ship]
    (manhattan-distance x y)))

(assert (= (solve-1 sample) 25))
(assert (= (solve-2 sample) 286))

(def input (utils/get-lines 12))

(assert (= (solve-1 input) 1148))
(assert (= (solve-2 input) 52203))

