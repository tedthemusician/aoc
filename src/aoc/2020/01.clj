(ns aoc.2020.01
  (:require [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [1721
             979
             366
             299
             675
             1456]) 

(defn makes-2020?
  "For use with `some`, no-op these numbers iff they add up to 2020"
  [nums]
  (if (= 2020 (reduce + nums)) nums nil))

(defn make-2020-finder
  "Make a function that finds [quantity] nums that add up to 2020"
  [quantity]
  (fn [nums] (some makes-2020? (combo/combinations nums quantity))))

(defn find-2020-addends
  "Given [quantity], find that many nums from [nums] that add up to 2020"
  [nums quantity]
  (reduce * ((make-2020-finder quantity) nums)))

(defn solve-1 [lines] (find-2020-addends lines 2))
(defn solve-2 [lines] (find-2020-addends lines 3))

(assert (= (solve-1 sample) 514579))
(assert (= (solve-2 sample) 241861950))

(def input (utils/get-read-lines 2020 1))

(assert (= (solve-1 input) 437931))
(assert (= (solve-2 input) 157667328))
