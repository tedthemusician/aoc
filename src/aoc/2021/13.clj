(ns aoc.2021.13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [["6,10"
              "0,14"
              "9,10"
              "0,3"
              "10,4"
              "4,11"
              "6,0"
              "6,12"
              "4,1"
              "0,13"
              "10,12"
              "3,4"
              "3,0"
              "8,4"
              "1,10"
              "2,14"
              "8,10"
              "9,0"]
             ["fold along y=7"
              "fold along x=5"]])

(defn parse-coord
  [s]
  (mapv edn/read-string (str/split s #",")))

(defn parse-fold
  [s]
  (let [[_ axis index] (re-matches #".*(\w)=(\d+)" s)]
    {:axis (keyword axis) :index (edn/read-string index)}))

(defn parse-groups
  [[coords folds]]
  {:coords (mapv parse-coord coords)
   :folds (map parse-fold folds)})

(defn show-line
  [width xs]
  (apply str (map #(if (contains? (set xs) %) \# \.) (range (inc width)))))

(defn show-matrix
  [coords]
  (let [width (apply max (map first coords))
        height (apply max (map second coords))
        y-groups (group-by second coords)
        ]
    (map (fn [y]
           (let [xs (map first (get y-groups y []))]
             (show-line width xs)))
         (range (inc height)))))

(defn print-matrix
  [m]
  (print (str/join "\n" m)))

(defn fold-n
  [index n]
  (if (> index n)
    n
    (- index (- n index))))

(defn fold-coord
  [{:keys [index axis]} [x y]]
  (if (= axis :x)
    [(fold-n index x) y]
    [x (fold-n index y)]))

(defn fold-matrix
  [m fold]
  (vec (distinct (map (partial fold-coord fold) m))))

(defn solve-1
  [groups]
  (let [{:keys [coords folds]} (parse-groups groups)]
    (count (fold-matrix coords (first folds)))))

(defn solve-2
  [groups]
  (let [{:keys [coords folds]} (parse-groups groups)
        final-matrix (reduce fold-matrix coords folds)]
    (show-matrix final-matrix)
    ; .##..####...##.#..#.#....#..#..##....##
    ; #..#.#.......#.#.#..#....#..#.#..#....#
    ; #....###.....#.##...#....#..#.#.......#
    ; #....#.......#.#.#..#....#..#.#.##....#
    ; #..#.#....#..#.#.#..#....#..#.#..#.#..#
    ; .##..####..##..#..#.####..##...###..##.
    "CEJKLUGJ"))

(utils/verify-solutions
  [{:method solve-1 :sample 17 :input 795}
   {:method solve-2 :input "CEJKLUGJ"}]
  {:value sample}
  (utils/get-line-groups 2021 13))
