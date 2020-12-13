(ns aoc.2020.06
  (:require [clojure.set :as set]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["abc"
             ""
             "a"
             "b"
             "c"
             ""
             "ab"
             "ac"
             ""
             "a"
             "a"
             "a"
             "a"
             ""
             "b"])


(defn solve-1
  "How many distinct letters are there per paragraph?"
  [groups]
  (reduce + (map (comp count set) (utils/groups->paragraphs groups ""))))

(defn num-common-elems
  [colls]
  "How many elements are common to every coll?"
  (count (apply set/intersection (map set colls))))

(defn solve-2
  "In each group, how many letters are common to all lines?"
  [groups]
  (reduce + (map num-common-elems groups)))

(def grouped-sample (utils/lines->groups sample))

(assert (= (solve-1 grouped-sample) 11))
(assert (= (solve-2 grouped-sample) 6))

(def input (utils/get-line-groups 2020 6))

(assert (= (solve-1 input) 6782))
(assert (= (solve-2 input) 3596))

