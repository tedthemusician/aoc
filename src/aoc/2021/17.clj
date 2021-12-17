(ns aoc.2021.17
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample "target area: x=20..30, y=-10..-5")
(def input "target area: x=169..206, y=-108..-68")

(defn parse-region
  [input]
  (let [[full-match & matches] (re-matches
                                 #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)"
                                 input)
        [x0 x1 y0 y1] (map edn/read-string matches)
        width (- x1 x0)
        height (- y1 y0)]
    {:x0 x0
     :y0 y0
     :x1 x1
     :y1 y1
     :width width
     :height height}))

(defn get-good-initial-xvels
  [{:keys [x0 x1]}]
  (let [termini (take (inc x1) (reductions + (range)))]
    (keep-indexed (fn [xvel terminus] (if (>= terminus x0) xvel)) termini)))

(defn get-good-initial-yvels
  [{:keys [y0]}]
  (range y0 (- y0)))

(defn get-candidate-trajectories
  [region]
  (for [xvel (get-good-initial-xvels region)
        yvel (get-good-initial-yvels region)]
    {:xvel xvel :yvel yvel}))

(defn get-xvels
  [initial-xvel]
  (lazy-seq (cons initial-xvel (get-xvels (max 0 (dec initial-xvel))))))

(defn get-yvels
  [initial-yvel]
  (lazy-seq (cons initial-yvel (get-yvels (dec initial-yvel)))))

(defn get-visited-values
  [vel-accumulator initial-vel]
  (cons 0 (reductions + (vel-accumulator initial-vel))))

(def get-visited-xs (partial get-visited-values get-xvels))
(def get-visited-ys (partial get-visited-values get-yvels))

(defn get-path
  [{:keys [xvel yvel]}]
  (map (fn [x y] {:x x :y y}) (get-visited-xs xvel) (get-visited-ys yvel)))

(defn get-relevant-path
  [region trajectory]
  (let [min-y (:y0 region)
        path (get-path trajectory)]
    (take-while #(>= (:y %) min-y) path)))

(defn in-region?
  [region point]
  (let [{:keys [x0 x1 y0 y1]} region
        {:keys [x y]} point]
    (and (>= x x0)
         (<= x x1)
         (>= y y0)
         (<= y y1))))

(defn enters-region?
  [region path]
  (some (partial in-region? region) path))

(defn traverse
  [region trajectory]
  (let [path (get-relevant-path region trajectory)
        max-y (apply max (map :y path))
        ]
    {:trajectory trajectory
     :enters-region? (enters-region? region path)
     :max-y max-y}))

(defn get-successful-trajectories
  [region]
  (let [candidates (get-candidate-trajectories region)
        data (map (partial traverse region) candidates)]
    (filter :enters-region? data)))

(defn solve-1
  [s]
  (let [region (parse-region s)
        successes (get-successful-trajectories region)]
    (:max-y (first (sort-by :max-y > successes)))))

(defn solve-2
  [s]
  (let [region (parse-region s)]
    (count (get-successful-trajectories region))))

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 45 :input 5778}
   {:method solve-2 :sample 112}]
  {:value sample}
  input)
