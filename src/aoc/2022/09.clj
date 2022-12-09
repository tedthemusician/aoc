(ns aoc.2022.09
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def sample ["R 4"
             "U 4"
             "L 3"
             "D 1"
             "R 4"
             "D 1"
             "L 5"
             "R 2"])

(def dirs {"U" [0 1]
           "D" [0 -1]
           "R" [1 0]
           "L" [-1 0]})

(defn parse-line
  "Parse a line into a direction and a number of times to travel one unit in
  that direction"
  [s]
  (let [[d n] (str/split s #" ")]
    {:dir (dirs d)
     :magnitude (edn/read-string n)}))

(def s (map parse-line sample))
(def initial-line {:head [0 0], :tail [0 0]})

(defn distance
  "Get the 'distance', a la D&D, between two points"
  [{:keys [head tail]}]
  (let [[hx hy] head
        [tx ty] tail]
    (max (utils/abs (- hx tx))
         (utils/abs (- hy ty)))))

(def move-end (partial map +))

(defn pull-line-once
  "Move head one unit in a given direction and ensure tail is one unit away"
  [dir starting-line]
  (let [starting-distance (distance starting-line)
        pulled-line (update starting-line :head (partial move-end dir))]
    (if (zero? starting-distance)
      pulled-line
      (let [pulled-distance (distance pulled-line)]
        (if (<= pulled-distance 1)
          pulled-line
          (assoc pulled-line :tail (:head starting-line)))))))

(defn pull-line-multiple
  "Move head multiple units in a given direction and ensure tail is always one
  unit away. Return the new position and all coordinates visited by tail."
  [{:keys [line tail-coords]} {:keys [dir magnitude]}]
  (let [intermediate-lines (rest (take (inc magnitude)
                                  (iterate (partial pull-line-once dir)
                                           line)))
        new-line (last intermediate-lines)
        tail-coords' (set (map :tail intermediate-lines))]
    {:line new-line :tail-coords (set/union tail-coords tail-coords')}))

(defn follow-path
  "Pull a line along a path given by instructions"
  [instructions]
  (reduce pull-line-multiple {:line initial-line :tail-coords #{}} instructions))

(count (:tail-coords (follow-path s)))

(defn solve-1
  [input]
  (->> input
       (map parse-line)
       follow-path
       :tail-coords
       count))

(defn solve-2
  [input]
  nil)

(utils/verify-solutions
  [{:method solve-1 :sample 13}
   #_ {:method solve-2 :sample nil :input nil}]
  {:value sample}
  (utils/get-lines 2022 9))
