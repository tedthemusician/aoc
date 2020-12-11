(ns aoc.10
  (:require [aoc.utils :as utils])
  (:gen-class))

(def sample-1 [16
               10
               15
               5
               1
               11
               7
               19
               6
               12
               4])

(def sample-2 [28
               33
               18
               42
               31
               14
               46
               20
               48
               47
               24
               23
               49
               45
               19
               38
               39
               11
               1
               32
               25
               35
               8
               17
               7
               9
               4
               2
               34
               10
               3])

(defn add-ends
  "Add the outlet (0) and our device (max + 3) to our list of adapters"
  [coll]
  (let [adapters (vec (sort coll))
        with-device (conj adapters (+ 3 (apply max adapters)))]
    (conj (apply list with-device) 0)))

(defn get-differences
  "Get the difference between each item and the next item"
  [coll]
  (map - (next coll) coll))

(defn get-rating-and-preds
  "Get all ratings in [coll] that are 1, 2, or 3 fewer than [x]. Return a
  one-item map so we can do this to an entire colleciton and merge them later."
  [coll x]
  {x (sort (filter #(and (< % x) (>= % (- x 3))) coll))})

(defn get-preds
  "Make a map of adapter ratings to the ratings they accept. Sort this map so
  we can process it in ascending order later."
  [coll]
  (into (sorted-map) (map (partial get-rating-and-preds coll) coll)))

(defn get-combo-counts
  "Get the number of combinations that can lead to each adapter. We add this up
  from the outlet up rather than from the device down for performance, as this
  way we only have to compute each sum once."
  [coll]
  (reduce-kv
    (fn [acc index preds]
      (if (empty? preds)
        (assoc acc index 1)
        (assoc acc index (reduce + (map #(get acc %) preds)))))
    {}
    coll))

(defn solve-1
  "Get our input sorted with the outlet and our device, get the differences
  between elements, count the amount of 1s and 3s in those differences, and
  multipy them"
  [nums]
  (let [with-ends (add-ends nums)
        diffs (get-differences with-ends)
        freqs (frequencies diffs)]
    (* (get freqs 1) (get freqs 3))))

(defn solve-2
  "How many ways can we connect our adapters from the outlet to our device?"
  [nums]
  (let [with-ends (add-ends nums)
        with-preds (get-preds with-ends)
        combo-counts (get-combo-counts with-preds)
        device-rating (apply max with-ends)]
    (get combo-counts device-rating)))

(assert (= (solve-1 sample-1) 35))
(assert (= (solve-1 sample-2) 220))

(assert (= (solve-2 sample-1) 8))
(assert (= (solve-2 sample-2) 19208))

(def input (utils/get-read-lines 10))

(assert (= (solve-1 input) 2080))
(assert (= (solve-2 input) 6908379398144))

