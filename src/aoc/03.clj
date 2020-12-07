(ns aoc.03
  (:require [aoc.utils :as utils])
  (:gen-class))

(def sample ["..##......."
             "#...#...#.."
             ".#....#..#."
             "..#.#...#.#"
             ".#...##..#."
             "..#.##....."
             ".#.#.#....#"
             ".#........#"
             "#.##...#..."
             "#...##....#"
             ".#..#...#.#"])

(defn val-at
  "Considering our row an infinite loop, is there a tree or nothing at [y * dx]?"
  [row dx y]
  (if (= (nth row (mod (* dx y) (count row))) \#)
    1
    0))

(defn get-sum 
  "How many trees did we hit at this heading, where only dx can change?"
  [rows dx]
  (reduce + (map-indexed #(val-at %2 dx %1) rows)))

(defn glide
  "How many trees did we hit at this heading, where both dx and dy can change?
  Any dy over 1 will allow us to drop some of the rows, since we'll never hit
  any of their trees."
  [rows dx dy]
  (get-sum (take-nth dy rows) dx))

(defn solve-1 [lines] (glide lines 3 1))

(defn solve-2
  "Get the product of the numbers of trees at each of these headings"
  [lines]
  (let [vels [[1 1]
              [3 1]
              [5 1]
              [7 1]
              [1 2]]]
    (reduce * (map (partial apply (partial glide lines)) vels))))

(assert (= (solve-1 sample) 7))
(assert (= (solve-2 sample) 336))

(def input (utils/get-lines 3))

(assert (= (solve-1 input) 195))
(assert (= (solve-2 input) 3772314000))
