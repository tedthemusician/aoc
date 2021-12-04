(ns aoc.2021.26
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [aoc.utils :as utils])
  (:gen-class))

(def sample nil)

(defn solve-1
  []
  nil)

(defn solve-2
  []
  nil)

(utils/show-results
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample :s1 :input :i1}
   {:method solve-2 :sample :s2}]
  {:value sample}
  (utils/get-read-lines 2021 26))
