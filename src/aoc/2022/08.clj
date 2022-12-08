(ns aoc.2022.08
  (:require [aoc.utils :as utils]
            [clojure.edn :as edn])
  (:gen-class))

(def sample ["30373"
             "25512"
             "65332"
             "33549"
             "35390"])

(defn parse-line
  "Get individual digits as ints from a line"
  [line]
  (vec (map (comp edn/read-string str)
             (seq line))))

(def resets
  {:cw   :ccw
   :180  :180
   :ccw  :cw
   :none :none})

(def coord-rotations
  {:cw   (fn [len row col] [col (dec (- len row))])
   :180  (fn [len row col] [(dec (- len row)) (dec (- len col))])
   :ccw  (fn [len row col] [(dec (- len col)) row])
   :none (fn [len row col] [row col])})

(def matrix-rotations
  {:cw   (fn [m] (map reverse (utils/transpose m)))
   :180  (fn [m] (map reverse (reverse m)))
   :ccw  (fn [m] (reverse (utils/transpose m)))
   :none identity})

(defn get-increasing-indices
  "Get the indices of elements that are greater than all preceding elements"
  [[x & xs]]
  (loop [indices (list 0)
         index 1
         highest x
         xs xs]
    (if (empty? xs)
      (reverse indices)
      (let [[curr & xs'] xs
            index' (inc index)]
        (if (> curr highest)
          (recur (cons index indices) index' curr xs')
          (recur indices index' highest xs'))))))

(defn get-visible-indices-from-left
  "Get a list of indices visible from the left side"
  [rows]
  (apply concat (map-indexed (fn [col-index row]
                               (map #(vector col-index %)
                                    (get-increasing-indices row)))
                             rows)))

(defn get-visible-indices-after-rotation
  "Get a list of indices visible from the left side after rotating m"
  [rotation m]
  (let [rotation-func (matrix-rotations rotation)
        m' (rotation-func m)
        coords (get-visible-indices-from-left m')
        counterrotation-func (coord-rotations (resets rotation))]
    (map #(apply counterrotation-func (count m) %) coords)))


(defn get-all-visible-coords
  "Get the coordinates of all indices visible from any side"
  [m]
  (distinct (apply concat (map #(get-visible-indices-after-rotation % m)
                               (keys matrix-rotations)))))

(defn get-coords-above
  "Get coords above (row, col) in descending order of row"
  [len row col]
  (reverse (map #(vector % col) (range 0 row))))

(defn get-coords-below
  "Get coords below (row, col) in ascending order of row"
  [len row col]
  (map #(vector % col) (range (inc row) len)))

(defn get-coords-leftward
  "Get coords left of (row, col) in descending order of col"
  [len row col]
  (reverse (map #(vector row %) (range 0 col))))

(defn get-coords-rightward
  "Get coords right of (row, col) in ascending order of col"
  [len row col]
  (map #(vector row %) (range (inc col) len)))

(defn get-view-length
  "Get the number of elements less than the value at (row, col), until and
  including the first value greater than or equal to the value at (row, col)"
  [m row col view-func]
  (let [n (get-in m [row col])
        coords (view-func (count m) row col)
        all-ns (map #(get-in m %) coords)
        visible-ns (take-while #(< % n) all-ns)]
    (if (= all-ns visible-ns)
      (count all-ns)
      (inc (count visible-ns)))))

(defn get-view-lengths
  "Get the view lengths in all cardinal directions from a coordinate"
  [m row col]
  (map #(get-view-length m row col %)
       [get-coords-above
        get-coords-below
        get-coords-leftward
        get-coords-rightward]))

(defn score-coord
  "Multiply the view lengths in all cardinal directions from a coordinate"
  [m row col]
  (reduce * (get-view-lengths m row col)))

(defn solve-1
  [input]
  (->> input
       (mapv parse-line)
       get-all-visible-coords
       count))

(defn solve-2
  [input]
  (let [m (mapv parse-line input)
        len (count m)
        coords (for [x (range len) y (range len)] [x y])
        scores (map #(apply score-coord m %) coords)]
    (apply max scores)))

(utils/verify-solutions
  [{:method solve-1 :sample 21 :input 1733}
   {:method solve-2 :sample 8 :input 284648}]
  {:value sample}
  (utils/get-lines 2022 8))
