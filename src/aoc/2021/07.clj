(ns aoc.2021.07
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [16 1 2 0 4 2 7 1 2 14])

(defn median
  "The middle element of a collection when sorted."
  [xs]
  (nth (sort xs) (int (/ (count xs) 2))))

(defn fuel-used
  "The sumtorial of the difference between start and stop. Zero if start and
  stop are the same."
  [start stop]
  (reduce + (range 1 (inc (Math/abs (- start stop))))))

(defn solve-1
  [nums]
  (let [med (median nums)]
    (reduce + (map #(Math/abs (- % med)) nums))))

(defn solve-2
  [nums]
  (let [low (apply min nums)
        high (apply max nums)
        candidates (range low (inc high))]
    (:total (reduce (fn [best curr]
               (let [differences (map (partial fuel-used curr) nums)
                     total (reduce + differences)]
                 (if (< total (:total best))
                   {:value curr :total total}
                   best)))
             {:value nil :total ##Inf} candidates))))

(utils/verify-solutions
  [{:method solve-1 :sample 37 :input 356922}
   {:method solve-2 :sample 168 :input 100347031}]
  {:value sample}
  (map edn/read-string (str/split (utils/get-text 2021 07) #",")))
