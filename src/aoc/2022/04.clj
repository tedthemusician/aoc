(ns aoc.2021.04
  (:require [aoc.utils :as utils]
            [clojure.string :as str])
  (:gen-class))

(def sample ["2-4,6-8"
             "2-3,4-5"
             "5-7,7-9"
             "2-8,3-7"
             "6-6,4-6"
             "2-6,4-8"])

(defn parse-range
  "Parse a hyphen-delimited range"
  [s]
  (utils/parse-nums s "-"))

(defn parse-line
  "Parse a commo-delimited duplet of ranges"
  [s]
  (map parse-range (str/split s #",")))

(defn redundant?
  "Is r1 fully within r2 or vice versa?"
  [r1 r2]
  (let [[m0 m1] r1
        [n0 n1] r2]
    (or (and (<= m0 n0) (>= m1 n1))
        (and (<= n0 m0) (>= n1 m1)))))

(defn overlap?
  "Do r1 and r2 share at least one element?"
  [r1 r2]
  (let [[m0 m1] r1
        [n0 n1] r2]
    (or (and (>= n0 m0) (<= n0 m1))
        (and (>= m0 n0) (<= m0 n1)))))

(defn solve
  [pred lines]
  (->> lines
       (map (comp (partial apply pred)
                  parse-line))
       (filter true?)
       count))

(def solve-1 (partial solve redundant?))
(def solve-2 (partial solve overlap?))


(utils/verify-solutions
  [{:method solve-1 :sample 2 :input 515}
   {:method solve-2 :sample 4 :input 883}]
  {:value sample}
  (utils/get-lines 2022 4))
