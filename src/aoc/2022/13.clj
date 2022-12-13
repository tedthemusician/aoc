(ns aoc.2022.13
  (:require [aoc.utils :as utils]
            [clojure.edn :as edn])
  (:gen-class))

(def sample [["[1,1,3,1,1]"
              "[1,1,5,1,1]"]
             ["[[1],[2,3,4]]"
              "[[1],4]"]
             ["[9]"
              "[[8,7,6]]"]
             ["[[4,4],4,4]"
              "[[4,4],4,4,4]"]
             ["[7,7,7,7]"
              "[7,7,7]"]
             ["[]"
              "[3]"]
             ["[[[]]]"
              "[[]]"]
             ["[1,[2,[3,[4,[5,6,7]]]],8,9]"
              "[1,[2,[3,[4,[5,6,0]]]],8,9]"]])

(def parse-input (partial map (partial map edn/read-string)))

(def comparators
  {true -1
   false 1})

(defn cmp-nums
  "Return true when left is greater than right, false when left is less than
  right, and nil when left and right are equal."
  [l r]
  (cond (< l r) true
        (> l r) false
        :otherwise nil))

(declare cmp-elems)

(defn cmp-lists
  "Return true when:
  - Left is empty and right is not
  - Left's first item is considered 'less' than right's
  Return false when:
  - Right is empty and left is not
  - Right's first entry is considered 'less' than left's
  If the first two items resolve identically, perform the same comparison on
  the remaining items of each list."
  [l r]
  (cond (= l r) nil
        (empty? l) true
        (empty? r) false
        :otherwise (let [[l0 & ls] l
                         [r0 & rs] r
                         result (cmp-elems l0 r0)]
                     (if (boolean? result)
                       result
                       (cmp-lists ls rs)))))

(defn cmp-elems
  "When l and r are both numbers, compare them directly.
  When l and r are both lists, compare them recursively.
  When only one of l or r is a list, convert the other into a list and then
  perform the comparison as described above."
  [l r]
  (cond (and (number? l) (number? r)) (cmp-nums l r)
        (and (coll? l) (coll? r)) (cmp-lists l r)
        (number? l) (cmp-lists [l] r)
        :otherwise (cmp-lists l [r])))

(defn solve-1
  [input]
  (->> input
       parse-input
       (utils/indices (partial apply cmp-elems))
       (map inc)
       (reduce +)))

(defn solve-2
  [input]
  (let [dividers [[[2]] [[6]]]]
   (->> input
        flatten
        (map edn/read-string)
        (concat dividers)
        (sort (comp comparators cmp-elems))
        (utils/indices (set dividers))
        (map inc)
        (reduce *))))

(solve-2 sample)

(utils/verify-solutions
  [{:method solve-1 :sample 13 :input 6568}
   {:method solve-2 :sample 140 :input 19493}]
  {:value sample}
  (utils/get-groups 2022 13))
