(ns aoc.2021.01
  (:require [aoc.utils :as utils])
  (:gen-class))

(def sample nil)

(def solve-1 identity)

(def solve-2 identity)

(utils/verify-solutions
  [{:method solve-1 :sample nil :input nil}
   #_ {:method solve-2 :sample nil :input nil}]
  {:value sample}
  (utils/get-read-lines 2022 1))
