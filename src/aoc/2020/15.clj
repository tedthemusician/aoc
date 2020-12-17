(ns aoc.2020.15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def samples-1 {"0,3,6" 436
                "1,3,2" 1
                "2,1,3" 10
                "1,2,3" 27
                "2,3,1" 78
                "3,2,1" 438
                "3,1,2" 1836})

(def samples-2 {"0,3,6" 175594
                "1,3,2" 2578
                "2,1,3" 3544142
                "1,2,3" 261214
                "2,3,1" 6895259
                "3,2,1" 18
                "3,1,2" 362})

(defn parse-nums
  "Parse a comma-separated list of numbers"
  [s]
  (map edn/read-string (str/split s #",")))

(defn get-first-times
  "Get a map of numbers to the most recent turn when they were spoken,
  excluding the last number because it's just been spoken."
  [first-nums]
  (reduce-kv
    (fn [acc index curr] (assoc acc curr index))
    {}
    (vec (drop-last first-nums))))

(defn play-game
  "After hearing a number, speak the number of turns that have elapsed since it
  was last spoken, or speak 0 if it has never been spoken. Continue for
  [num-turns] turns. This is an indexed loop for the sake of performance."
  [first-nums num-turns]
  (let [first-i (dec (count first-nums))
        max-i (- num-turns 2)
        first-num (last first-nums)
        first-times (get-first-times first-nums)]
    (loop [i first-i, curr-num first-num, times first-times]
      (let [last-time (get times curr-num)
            next-num (if (some? last-time) (- i last-time) 0)]
        (if (= i max-i)
          next-num
          (recur (inc i) next-num (assoc times curr-num i)))))))

(defn make-solver
  "Make a function that gets the [n]th number spoken, stariting with the input"
  [n]
  (fn [text] (play-game (utils/parse-nums text) n)))

(def solve-1 (make-solver 2020))
(def solve-2 (make-solver 30000000))

(assert (every? #(= (solve-1 (first %)) (second %)) samples-1))
(assert (every? #(= (solve-2 (first %)) (second %)) samples-2))

(def input "9,6,0,10,18,2,1")

(assert (= (solve-1 input) 1238))
(assert (= (solve-2 input) 3475954))

