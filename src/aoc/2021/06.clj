(ns aoc.2021.06
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [3 4 3 1 2])

(defn init-counts
  "Create a vector of 9 elements, each of which represents how many numbers are
  at the nth place"
  [nums]
  (reduce (fn [freqs n]
            (assoc freqs n (inc (nth freqs n))))
          (vec (repeat 9 0))
          nums))

(defn tick
  "Decrement a number. Wrap negatives to 6."
  [n]
  (if (zero? n) 6 (dec n)))

(defn iter
  "Rotate counts left and add the existing value at position zero to the new
  value at position six"
  [counts]
  (let [[nzeros & positives] counts
        rotated (vec (concat positives [nzeros]))
        nsixes (nth rotated 6)]
    (assoc rotated 6 (+ nsixes nzeros))))

(defn solve
  [ndays nums]
  (reduce + (nth (iterate iter (init-counts nums)) ndays)))

(defn solve-1
  [nums]
  (solve 80 nums))

(defn solve-2
  [nums]
  (solve 256 nums))

(utils/verify-solutions
  [{:method solve-1 :sample 5934 :input 350605}
   {:method solve-2 :sample 26984457539 :input 1592778185024}]
  {:value sample}
  (map edn/read-string (str/split (utils/get-text 2021 06) #",")))
