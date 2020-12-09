(ns aoc.09
  (:require [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["35"
             "20"
             "15"
             "25"
             "47"
             "40"
             "62"
             "55"
             "65"
             "95"
             "102"
             "117"
             "150"
             "182"
             "127"
             "219"
             "299"
             "277"
             "309"
             "576"])

(defn valid?
  "Is [x] the sum of two distinct numbers in [coll?]"
  [x coll]
  (let [combos (combo/combinations coll 2)
        sums (map (partial apply +) combos)]
    (contains? (set sums) x)))

(defn first-invalid
  "Find the first number in the list that is not the sum of two distinct
  numbers of the prior [pre-len] numbers"
  [pre-len coll]
  (let [[preamble payload] (split-at pre-len coll)]
    (loop [current (reverse preamble), upcoming payload]
      (let [x (first upcoming)]
        (if (valid? x current)
          (recur (take pre-len (conj current x)) (next upcoming))
          x)))))

(defn all-takes
  "Every subsequence of [coll] that starts at the beginning"
  [coll]
  (map #(take % coll) (range 1 (inc (count coll)))))

(defn all-drops
  "Every subsequence of [coll] that ends at the end"
  [coll]
  (map #(drop % coll) (range (count coll))))

(defn all-subseqs
  "Every subsequence of [coll]"
  [coll]
  (mapcat all-drops (all-takes coll)))

(defn sequential-addends
  "Find the sequential numbers of [coll] that add up to [x]"
  [x coll]
  (first (filter #(= x (reduce + %)) (all-subseqs coll))))

(defn solve-1
  "What's the first invalid number in our input?"
  [pre-len lines]
  (first-invalid pre-len (map edn/read-string lines)))

(defn solve-2
  "What is the sum of the smallest and largest of the sequential numbers in our
  input that add up to the first invalid number in our input?"
  [pre-len lines]
  (let [numbers (map edn/read-string lines)
        invalid (first-invalid pre-len numbers)
        addends (sequential-addends invalid numbers)]
    (+ (apply min addends) (apply max addends))))

(assert (= (solve-1 5 sample) 127))
(assert (= (solve-2 5 sample) 62))

(def input (utils/get-lines 9))

(assert (= (solve-1 25 input) 27911108))
(assert (= (solve-2 input) 4023754))

