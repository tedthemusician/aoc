(ns aoc.2020.13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample-1 ["939"
             "7,13,x,x,59,x,31,19"])

(def samples-2 {"17,x,13,19" 3417
                "67,7,59,61" 754018
                "67,x,7,59,61" 779210
                "67,7,x,59,61" 1261476
                "1789,37,47,1889" 1202161486})

(defn parse-notes
  "How early can we catch a bus? What are the buses' inverals?"
  [[l1 l2]]
  (let [start-time (edn/read-string l1)
        strs (str/split l2 #",")
        intervals (map edn/read-string (filter #(not= % "x") strs))]
    {:start-time start-time :intervals intervals}))

(defn departure-times
  "When does a bus leave the station?"
  [interval]
  (map (partial * interval) (range)))

(defn first-departure-time
  "What's the earliest time we could board [interval] after [start]?"
  [start-time interval]
  (first (filter #(>= % start-time) (departure-times interval))))

(defn first-departure-times
  "Make a map of earliest departure times to bus intervals, sorted by
  departure time"
  [start-time intervals]
  (let [pairs (map #(vector (first-departure-time start-time %) %) intervals)]
    (into (sorted-map) pairs)))

(defn earliest-bus
  "Which bus picks us up the earliest, and what time does it arrive?"
  [start-time intervals]
  (let [[arrival-time interval]
        (first (first-departure-times start-time intervals))]
    {:arrival-time arrival-time :interval interval}))

(defn parse-intervals
  "Get the bus intervals and their offsets"
  [s]
  (let [strs (str/split s #",")]
    (keep-indexed
      (fn [i s] (if (= "x" s)
                  nil
                  {:interval (edn/read-string s)
                   :offset i}))
      strs)))

(defn gcd [a b]
  "Greatest common factor"
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Least common multiple"
  [a b]
  (/ (* a b) (gcd a b)))

(defn fill-gap
  "Given two times and an interval, add the interval to the lower time until
  it matches or exceeds the higher time"
  [low-t high-t interval]
  (let [difference (- high-t low-t)
        ratio (/ difference interval)]
    (if (int? ratio)
      (+ low-t difference)
      (+ low-t (* (long (Math/ceil ratio)) interval)))))

(defn first-match
  "Find the offset of this cycle, i.e. how long after t = 0 both buses arrive"
  [i1 offset-1 i2 offset-2]
  (loop [t1 offset-1 t2 (- offset-2)]
    (cond
      (= t1 t2) t1
      (< t1 t2) (recur (fill-gap t1 t2 i1) t2)
      (> t1 t2) (recur t1 (fill-gap t2 t1 i2)))))

(defn get-next-cycle
  "Find the offset and interval at which these two cycles coincide"
  [acc curr]
  (let [{i1 :interval offset-1 :offset} acc
        {i2 :interval offset-2 :offset} curr]
    {:interval (lcm i1 i2)
     :offset (first-match i1 offset-1 i2 offset-2)}))

(defn find-timestamp
  "At what timestamp to the intervals in [cycles] converge to a one-after-the-
  other sequence?"
  [cycles]
  (:offset (reduce get-next-cycle cycles)))

(defn solve-1
  "What's the procut of the earliest bus we can take and how long we have to
  wait?"
  [lines]
  (let [{:keys [start-time intervals]} (parse-notes lines)
        {:keys [arrival-time interval]} (earliest-bus start-time intervals)]
    (* interval (- arrival-time start-time))))

(defn solve-2
  "At what timestamp do the intervals in the second line converge to a one-
  after-the-other sequence?"
  [lines]
  (find-timestamp (parse-intervals (second lines))))

(defn validate [& args]
  (let [input (utils/get-lines 2020 13)]
    (do
      (assert (= (solve-1 sample-1) 295))
      (assert (= (solve-2 sample-1) 1068781))
      (assert (every? #(= (solve-2 ["" (first %)]) (second %)) samples-2))
      (assert (= (solve-1 input) 4808))
      (assert (= (solve-2 input) 741745043105674)))))

