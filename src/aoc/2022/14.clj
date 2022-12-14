(ns aoc.2022.14
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set])
  (:gen-class))

(def sample ["498,4 -> 498,6 -> 496,6"
             "503,4 -> 502,4 -> 502,9 -> 494,9"])

(def sand-origin [500 0])

(defn parse-point
  "Get the point represented by 'x,y'"
  [s]
  (map edn/read-string (str/split s #",")))

(defn build-edge
  "Get all points between x0,y0 and x1,y1"
  [[x0 y0] [x1 y1]]
  (for [x (range (min x0 x1) (inc (max x0 x1)))
        y (range (min y0 y1) (inc (max y0 y1)))]
    [x y]))

(defn parse-line
  "Get all edge points described by corners x0,y0 -> x1,y1 -> ... -> xn, yn"
  [line]
  (let [points (map parse-point (str/split line #" -> "))
        pairs (map vector points (rest points))]
    (set (mapcat (partial apply build-edge) pairs))))

(defn parse-input
  "Get all shapes described by their corners"
  [input]
  (set (mapcat parse-line input)))

(defn get-candidates
  "Get the three points directly below a line, <=1 away in the x direction"
  [[x y]]
  (map #(vector (% x) (inc y)) [identity dec inc]))

(defn drop-grain-1
  "Drop a grain until it can no longer move. If it falls indefinitely, return
  nil."
  [max-y covered-points [x y :as pos]]
  (if (> y max-y)
    nil
    (let [candidates (get-candidates pos)
          new-point (utils/first-by #(not (contains? covered-points %)) candidates)]
      (if (nil? new-point)
        pos
        (recur max-y covered-points new-point)))))

(defn pour-sand-1
  "Pour sand until grains start to fall indefinitely"
  [solid-points]
  (let [max-y (apply max (map second solid-points))]
    (loop [grains #{}]
      (let [new-grain (drop-grain-1 max-y (set/union solid-points grains) sand-origin)]
        (if (nil? new-grain)
          grains
          (recur (conj grains new-grain)))))))

(defn drop-grain-2
  "Drop a grain until it reaches the maximum depth"
  [max-y covered-points [x y :as pos]]
  (if (>= y max-y)
    pos
    (let [candidates (get-candidates pos)
          new-point (utils/first-by #(not (contains? covered-points %)) candidates)]
      (if (nil? new-point)
        pos
        (recur max-y covered-points new-point)))))

(defn pour-sand-2
  "Drop sand until it reaches the origin"
  [solid-points]
  (let [max-y (inc (apply max (map second solid-points)))]
    (loop [grains #{}]
      (let [new-grain (drop-grain-2 max-y (set/union solid-points grains) sand-origin)
            new-grains (conj grains new-grain)]
        (if (= sand-origin new-grain)
          new-grains
          (recur new-grains))))))

(defn solve-1
  [input]
  (->> input
       parse-input
       pour-sand-1
       count))

(defn solve-2
  [input]
  (->> input
       parse-input
       pour-sand-2
       count))

(utils/verify-solutions
  [{:method solve-1 :sample 24 :input 964}
   {:method solve-2 :sample 93 :input 32041}]
  {:value sample}
  (utils/get-lines 2022 14))
