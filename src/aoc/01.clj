(ns aoc.01
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["1721"
             "979"
             "366"
             "299"
             "675"
             "1456"]) 

(defn makes-2020?
  "Do these numbers add up to 2020?"
  [nums]
  (if (= 2020 (reduce + nums)) nums nil))

(defn make-2020-finder
  "Find [quantity] nums that add up to 2020 in a collection"
  [quantity]
  (fn [nums] (some makes-2020? (combo/combinations nums quantity))))

(defn solve
  "Given a quantity, solve the input that comes from utils/get-lines for today"
  [lines quantity]
  (let [all-nums (map edn/read-string lines)
        addends ((make-2020-finder quantity) all-nums)]
    (reduce * addends)))

(defn solve-1 [lines] (solve lines 2))
(defn solve-2 [lines] (solve lines 3))

(assert (= (solve-1 sample) 514579))
(assert (= (solve-2 sample) 241861950))

(def input (utils/get-lines 1))

(assert (= (solve-1 input) 437931))
(assert (= (solve-2 input) 157667328))
