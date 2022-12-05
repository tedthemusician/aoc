(ns aoc.2022.01
  (:require [aoc.utils :as utils])
  (:gen-class))

(def sample [[1000 2000 3000]
             [4000]
             [5000 6000]
             [7000 8000 9000]
             [10000]])

(defn solve-1
  [nums]
  (->> nums
       (map (partial reduce +))
       (apply max)))

(defn solve-2
  [nums]
  (->> nums
       (map (partial reduce +))
       sort
       (take-last 3)
       (reduce +)))

(utils/verify-solutions
  [{:method solve-1 :sample 24000 :input 68802}
   {:method solve-2 :sample 45000 :input 205370}]
  {:value sample}
  (utils/get-read-groups 2022 1))
