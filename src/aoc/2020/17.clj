(ns aoc.2020.17
  (:require [clojure.math.combinatorics :refer [selections]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [".#."
             "..#"
             "###"])

(def parse-line (partial mapv #(if (= % \#) 1 0)))

(defn parse-lines
  "Get an initial plane from [lines] embedded in [num-dimensions] dimensions"
  [num-dimensions lines]
  (let [rows (mapv parse-line lines)]
    (last (take (dec num-dimensions) (iterate vector rows)))))

(defn dimensions
  "Get the length of each dimension in [space]"
  [space]
  (loop [subspace space, dims []]
    (let [this-length (count subspace)]
      (if (coll? (first subspace))
        (recur (first subspace) (conj dims this-length))
        (conj dims this-length)))))

(defn pad-row
  "Add a cell of 0 to the left and right of a row"
  [row]
  (vec (concat [0] row [0])))

(defn pad-plane
  "Add a row of 0s above and below a plane"
  [plane]
  (let [padded-rows (map pad-row plane)
        width (count (first padded-rows))
        padding (vec (repeat width 0))]
    (vec (concat [padding] padded-rows [padding]))))

(defn pad-block
  "Add a plane of 0s in front of and behind a block"
  [block]
  (let [padded-planes (map pad-plane block)
        [height width] (dimensions (first padded-planes))
        padding (vec (repeat height (vec (repeat width 0))))]
    (vec (concat [padding] padded-planes [padding]))))

(defn pad-space
  "Add a block of 0s before and after a hypercube"
  [space]
  (let [padded-blocks (map pad-block space)
        [depth height width] (dimensions (first padded-blocks))
        padding (vec (repeat depth
                             (vec (repeat height
                                          (vec (repeat width 0))))))]
  (vec (concat [padding] padded-blocks [padding]))))

(defn neighbor-coords
  "Get every point adjacent to [coords], including along diagonals"
  [coords]
  (let [num-dimensions (count coords)
        offsets (selections [-1 0 1] num-dimensions)
        neighbors (map #(map (partial +) coords %) offsets)]
    (remove (partial = coords) neighbors)))

(def neighbor-coords-memo (memoize neighbor-coords))

(defn out-of-bounds?
  "Does this point lie outside of [space]?"
  [space coords]
  (let [dims (reverse (dimensions space))]
    (some
      identity
      (map (fn [c d] (or (neg? c) (>= c d))) coords dims))))

(defn state-at
  "The value in [space] at [coords]"
  [space coords]
  (if (out-of-bounds? space coords)
    0
    (loop [subspace space, coords (reverse coords)]
      (if (empty? coords)
        subspace
        (recur (nth subspace (first coords)) (rest coords))))))

(defn neighbor-vals
  "The value of every neighbor of [coords]"
  [space coords]
  (map (partial state-at space) (neighbor-coords-memo coords)))

(defn num-living-neighbors
  "The number of living neighbors at [coords]"
  [space coords]
  (apply + (neighbor-vals space coords)))

; TODO: See if we can generalize stepping through an n-dimensional space

(defn step-cell [space state & coords]
  "Apply the rules of Conway's game of life to this cell"
  (let [neighbor-count (num-living-neighbors space coords)]
    (cond
      (and (= state 1) (or (= neighbor-count 2) (= neighbor-count 3))) 1
      (and (= state 0) (= neighbor-count 3)) 1
      :else 0)))

(defn step-row-3d [block row y z]
  (vec (map-indexed (fn [x state] (step-cell block state x y z)) row)))

(defn step-plane-3d [block plane z]
  (vec (map-indexed (fn [y row] (step-row-3d block row y z)) plane)))

(defn step-block-3d [block]
  (vec (map-indexed (fn [z plane] (step-plane-3d block plane z)) block)))


(defn step-row-4d [space row y z w]
  (vec (map-indexed (fn [x state] (step-cell space state x y z w)) row)))

(defn step-plane-4d [space plane z w]
  (vec (map-indexed (fn [y row] (step-row-4d space row y z w)) plane)))

(defn step-block-4d [space block w]
  (vec (map-indexed (fn [z plane] (step-plane-4d space plane z w)) block)))

(defn step-space-4d [space]
  (vec (map-indexed (fn [w block] (step-block-4d space block w)) space)))


(defn iter-3d [space] (step-block-3d (pad-block space)))

(defn iter-4d [space] (step-space-4d (pad-space space)))

(defn num-living [space]
  "The number of living cells in the current state of [space]"
  (reduce + (flatten space)))

; (defn num-living-after-6 [space iter-func]
;   (let [num-dimensions (count (dimensions space))]
;     (num-living (first (drop 6 (iterate iter-func space))))))

(defn num-living-after-n
  "The number of living cells in [space] after [n] applications of [iter-func]"
  [n space iter-func]
  (loop [i 0, space space]
    (if (= i n)
      (num-living space)
      (recur (inc i) (iter-func space)))))

(defn solve-1
  "The number of living cells after 6 iterations in 3D space"
  [lines]
  (num-living-after-n 6 (parse-lines 3 lines) iter-3d))

(defn solve-2
  "The number of living cells after 6 iterations in 4D space"
  [lines]
  (num-living-after-n 6 (parse-lines 4 lines) iter-4d))

(defn validate [& args]
  (let [input (utils/get-lines 2020 17)]
    (do
      (assert (= (solve-1 sample) 112))
      (assert (= (solve-1 input) 391))
      (assert (= (solve-2 sample) 848))
      (assert (= (solve-1 input) 2264)))))

