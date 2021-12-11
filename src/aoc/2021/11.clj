(ns aoc.2021.11
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [[5 4 8 3 1 4 3 2 2 3] 
             [2 7 4 5 8 5 4 7 1 1] 
             [5 2 6 4 5 5 6 1 7 3] 
             [6 1 4 1 3 3 6 1 4 6] 
             [6 3 5 7 3 8 5 4 7 8] 
             [4 1 6 7 5 2 4 6 4 5] 
             [2 1 7 6 8 4 1 7 2 1] 
             [6 8 8 2 8 8 1 1 3 4] 
             [4 8 4 6 8 4 8 5 5 4] 
             [5 2 8 3 7 5 1 5 2 6]])

(defn mapm
  "Map a function across all values of a matrix."
  [f m]
  (mapv (partial mapv f) m))

(defn f-at
  "Apply `f` to point `p` in matrix `m`"
  [f m {:keys [x y]}]
  (update m y #(update % x f)))

(defn parse-line
  "Parse individual digits from a line of contiguous digits."
  [line]
  (mapv edn/read-string (str/split line #"")))

(defn get-flash-indices
  "Every index of vector `v` whose value is >10."
  [v]
  (keep-indexed (fn [index value]
                  (if
                    (and (some? value) (>= value 10))
                    index
                    nil))
                v))

(defn get-flash-points
  "Every point in matrix `m` whose value is 10."
  [m]
  (apply concat
         (filter not-empty
                 (map-indexed
                   (fn [y row]
                     (map (fn [x] {:x x :y y}) (get-flash-indices row)))
                   m))))

(defn get-neighbors
  "Every neighbor of point (`x`, `y`) for which 0 <= `x` < `w` & 0 <= `y` < `h`"
  [w h {:keys [x y]}]
  (let [xs (filter #(and (not (neg? %)) (< % w)) [(dec x) x (inc x)])
        ys (filter #(and (not (neg? %)) (< % h)) [(dec y) y (inc y)])
        all-points (for [y ys x xs] {:x x :y y})]
    (remove #(and (= x (:x %)) (= y (:y %))) all-points)))

(defn get-neighbors-of
  "All neighbors of points `ps` for which 0 <= `x` < `w` && 0 <= `y` < `h`.
  Can include the same neighbor multiple times."
  [m ps]
  (let [w (count (first m))
        h (count m)]
  (mapcat (partial get-neighbors w h) ps)))

(defn tick
  "Convert all 10s in matrix `m` to nils. Increment all non-nil neighbors of
  10s. Cells are incremented for every neighboring 10."
  [m]
  (let [flash-points (get-flash-points m)
        neighbors (get-neighbors-of m flash-points)
        flashed-m (reduce (partial f-at (constantly nil)) m flash-points)]
    (reduce (partial f-at #(if (nil? %) nil (inc %))) flashed-m neighbors)))

(defn step
  "Increment every value in matrix `m`, then perform tick until it reaches a
  fixed point.  Convert all remaining nils to 0s."
  [m]
  (->> m
       (mapm inc)
       (utils/fix tick)
       (mapm (fnil identity 0))))

(defn get-steps
  "Get `n` iterations of `step` applied to matrix `m.`"
  [n m]
  (take (inc n) (iterate step m)))

(defn solve-1
  [m]
  (->> m
       (get-steps 100)
       flatten
       (filter zero?)
       count))

(defn solve-2
  [m]
  (->> m
       (iterate step)
       (utils/find-index #(every? zero? (flatten %)))))

(utils/verify-solutions
  [{:method solve-1 :sample 1656 :input 1625}
   {:method solve-2 :sample 195 :input 244}]
  {:value sample}
  (mapv parse-line (utils/get-lines 2021 11)))
