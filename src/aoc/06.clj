(ns aoc.06
  (:require [clojure.string :as str]
            [clojure.set :as set]
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


(defn solve-1 [groups]
  "How many distinct letters are there per paragraph?"
  (reduce + (map (comp count set) (utils/groups->paragraphs groups ""))))

(defn num-common-elems
  "How many elements are common to every coll?"
  [colls]
  (count (apply set/intersection (map set colls))))

(defn solve-2 [groups]
  "In each group, how many letters are common to all lines?"
  (reduce + (map num-common-elems groups)))

(def grouped-sample (utils/lines->groups sample))

(assert (= (solve-1 grouped-sample) 11))
(assert (= (solve-2 grouped-sample) 6))

(def input (utils/get-line-groups 6))

(assert (= (solve-1 input) 6782))
(assert (= (solve-2 input) 3596))

