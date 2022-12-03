(ns aoc.2021.01
  (:require [aoc.utils :as utils]
            [clojure.set :as set])
  (:gen-class))

(def sample ["vJrwpWtwJgWrhcsFMMfFFhFp"
             "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
             "PmmdzqPrVvPwwTWBwg"
             "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
             "ttgJtRGJQctTZtZT"
             "CrZsJsPPZsGzwwsLwLmpwMDw"])

(def priorities
  (zipmap (concat (map char (range 97 123))
                  (map char (range 65 91)))
          (range 1 53)))

(defn parse-line
  [s]
  (let [mid (/ (count s) 2)]
    [(subs s 0 mid) (subs s mid)]))

(defn common-letter
  "Find the letter common to each string in a list of strings"
  [[s & ss]]
  (loop [common (set s), remaining (map set ss)]
    (if (empty? remaining) 
      (first common)
      (recur (set/intersection common (first remaining))
             (rest remaining)))))

(defn score-line
  "Get the priority of the letter common to each half of a string"
  [line]
  (priorities (common-letter (parse-line line))))

(defn solve-1
  [lines]
  (->> lines
       (map score-line)
       (reduce +)))

(defn solve-2
  [lines]
  (->> lines
       (partition 3)
       (map (comp priorities common-letter))
       (reduce +)))

(utils/verify-solutions
  [{:method solve-1 :sample 157 :input 7845}
   {:method solve-2 :sample 70 :input 2790}]
  {:value sample}
  (utils/get-lines 2022 3))
