(ns aoc.2022.09
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def samples [["R 4"
               "U 4"
               "L 3"
               "D 1"
               "R 4"
               "D 1"
               "L 5"
               "R 2"]
              ["R 5"
               "U 8"
               "L 8"
               "D 3"
               "R 17"
               "D 10"
               "L 25"
               "U 20"]])

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

(defn distance
  "Get the 'distance', a la D&D, between two points, such that diagonal
  distance is the longest lateral distance."
  [leader follower]
  (let [[hx hy] leader
        [tx ty] follower]
    (max (utils/abs (- hx tx))
         (utils/abs (- hy ty)))))

(def add-vectors (partial mapv +))

(defn get-change
  "Get the change necessary to apply to follower in order to ensure it stays
  next to leader"
  [leader' follower]
  (let [[dx dy] (mapv - leader' follower)]
    [(Integer/signum dx) (Integer/signum dy)]))

(defn diagonal?
  "Are two points diagonal from each other?"
  [[x1 y1] [x2 y2]]
  (and (not= x1 x2) (not= y1 y2)))

(defn avg [a b] (/ (+ a b) 2))

(defn pull-linear
  "Pull follower toward leader along x or y axis"
  [[x1 y1] [x2 y2]]
  [(avg x1 x2) (avg y1 y2)])

(defn pull
  "Pull follower to meet leader' according to the distance between leader' and
  leader such that follower stays one unit away. If leader' is more than one
  unit away from follower:
  - If leader and follower were originally diagonal, move follower to leader
  - Otherwise, move follower in the same manner as leader-to-leader'"
  [leader leader' follower]
  (let [starting-distance (distance leader follower)]
    (let [pulled-distance (distance leader' follower)]
      (cond (<= pulled-distance 1) follower
            (diagonal? leader' follower) (add-vectors follower (get-change leader' follower))
            :otherwise (pull-linear follower leader')))))

(defn drag-once
  "Pull the head of a chain one unit horizontally, vertically, or digaonally,
  and pull all following nodes one by one."
  [dir [leader & remaining]]
  (let [leader' (add-vectors leader dir)]
    (loop [leader leader
           leader' leader'
           remaining remaining
           new-points (list leader')]
      (if (empty? remaining)
        (reverse new-points)
        (let [[follower & remaining'] remaining
              follower' (pull leader leader' follower)
              new-points' (cons follower' new-points)]
          (recur follower follower' remaining' new-points'))))))

(defn drag-multiple
  "Drag head of chain multiple units. Return new node points and all points
  visited by tail."
  [{:keys [chain tail-coords]} {:keys [dir magnitude]}]
  (let [intermediate-chains (rest (take (inc magnitude)
                                  (iterate (partial drag-once dir) chain)))
        new-chain (last intermediate-chains)
        tail-coords' (set (map last intermediate-chains))]
    {:chain new-chain :tail-coords (set/union tail-coords tail-coords')}))

(defn follow-path
  "Pull a line along a path given by instructions"
  [initial-chain instructions]
  (reduce drag-multiple
          {:chain initial-chain, :tail-coords #{[0 0]}}
          instructions))

(defn solve
  [chain-len input]
  (->> input
       (map parse-line)
       (follow-path (repeat chain-len [0 0]))
       :tail-coords
       count))

(def solve-1 (partial solve 2))
(def solve-2 (partial solve 10))

(utils/verify-solutions
  [{:method solve-1 :sample [13 88] :input 6209}
   {:method solve-2 :sample [1 36] :input 2460}]
  {:multiple samples}
  (utils/get-lines 2022 9))
