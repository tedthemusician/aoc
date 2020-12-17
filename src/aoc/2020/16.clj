(ns aoc.2020.16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample-1 [["class: 1-3 or 5-9"
                "row: 6-11 or 33-44"
                "seat: 13-40 or 45-50"]
               ["your ticket:"
                "7,1,14"]
               ["nearby tickets:"
                "7,3,47"
                "40,4,50"
                "55,2,20"
                "38,6,12"]])

(def rule-pattern #"^(\D+): (\d+)-(\d+) or (\d+)-(\d+)$")

(defn parse-rule
  "Get the name of a rule and the two sets of bounds for its acceptable values"
  [s]
  (let [[_ rule-name & nums] (re-matches rule-pattern s)
        [min-1 max-1 min-2 max-2] (map edn/read-string nums)]
    [rule-name [{:min min-1 :max max-1} {:min min-2 :max max-2}]]))

(defn parse-input [[rule-group your-group others-group]]
  "Get rules, your ticket's values, and other tickets' values"
  {:rules (into {} (map parse-rule rule-group))
   :your-ticket (utils/parse-nums (second your-group))
   :other-tickets (map utils/parse-nums (rest others-group))})

(defn surrounds?
  "Does [r2] contain all the indices of [r1]?"
  [r1 r2]
  (and (<= (:min r2) (:min r1)) (>= (:max r2) (:max r1))))

(defn overlaps?
  "Do [r1] and [r2] contain common or adjacent indices, assuming r1's :min is
  lower than r2's :min?"
  [r1 r2]
  (>= (:max r1) (dec (:min r2))))

(defn within-range?
  "Does [n] lie within [r]? We compute this with a min and max rather than
  constructing a range for the sake of performance."
  [r n]
  (and (>= n (:min r)) (<= n (:max r))))

(defn within-ranges?
  "Does [n] lie within any of the ranges [rs]?"
  [rs n]
  (some #(within-range? % n) rs))

(def sort-ranges (partial sort-by (juxt :min :max)))

(defn merge-ranges-if-overlapping
  "If [r1] and [r2] have any elements in common, or if they have adjacent elements,
  merge them into one range and return it in a vector. Otherwise return a
  vector of both arguments unchanged."
  [r1 r2]
  (let [[lower higher] (sort-ranges [r1 r2])]
    (cond
      (surrounds? r1 r2) [r2]
      (overlaps? r1 r2) [{:min (:min lower) :max (:max higher)}]
      :else [lower higher])))

(defn merge-with-last-range
  "Merge [r] with the last range of [rs], then replace that last range with
  the result. If the two ranges don't overlap, this just appends [r] to [rs]."
  [rs r]
  (let [merged (merge-ranges-if-overlapping (last rs) r)]
    (concat (drop-last rs) merged)))

(defn merge-consecutive-ranges
  "Merge as many ranges in [rs] as possible"
  [rs]
  (let [[first-r & sorted-rs] (sort-ranges rs)]
   (reduce merge-with-last-range [first-r] sorted-rs)))

(defn satisfies-rule?
  "Does [n] satisfy any of [rules]?"
  [rules n]
  (let [all-ranges (flatten (vals rules))
        merged-ranges (merge-consecutive-ranges all-ranges)]
    (within-ranges? merged-ranges n)))

(defn valid?
  "Does every value on [ticket] satsify at least one of [rules]?"
  [rules ticket]
  (every? (partial satisfies-rule? rules) ticket))

(defn bad-values
  "The numbers in [nums] that don't lie within any of the ranges in [rules]?"
  [rules nums]
  (remove (partial satisfies-rule? rules) nums))

(defn error-rate
  "The sum of all values on [tickets] that don't lie within any of the ranges
  in [rules]"
  [rules tickets]
  (reduce + (mapcat (partial bad-values rules) tickets)))

(defn good-tickets
  "Keep only tickets from [tickets] considered valid according to [rules]"
  [rules tickets]
  (filter (partial valid? rules) tickets))

(defn rule-matches?
  "Does every value in [values] match the ranges in a rule?"
  [[rule-name ranges] values]
  (every? (partial within-ranges? ranges) values))

(defn matching-rule-names
  "Get every rule name from [rules] name that can be satisfied by every value
  in [values]"
  [rules values]
  (map first (filter #(rule-matches? % values) rules)))

(defn matching-rule-sets
  "Get every set of rules from [rules] that can be satisfied by each group of
  values in [value-groups]"
  [rules value-groups]
  (map (partial matching-rule-names rules) value-groups))

(defn remove-used
  "Given a list of [used-names], remove those names from [candidate-names]"
  [used-names candidate-names]
  (if (> (count candidate-names) 1)
    (remove #(contains? (set used-names) %) candidate-names)
    candidate-names))

(defn eliminate-bad-rules
  "Given [rule-sets], identify which rule sets have only one element; only
  that rule can apply to that index. Remove those names from other rule sets.
  Recur until all rule sets contain only one element."
  [rule-sets]
  (let [singletons (flatten (filter #(= 1 (count %)) rule-sets))
        dupes-removed (map (partial remove-used singletons) rule-sets)]
    (if (every? #(= 1 (count %)) dupes-removed)
      dupes-removed
      (recur dupes-removed))))

(defn order-rules
  "Derive the order of [rules] according to the ranges of values at each index
  on [tickets]"
  [rules tickets]
  (let [value-groups (utils/transpose tickets)
        rule-sets (matching-rule-sets rules value-groups)
        valid-name-groups (eliminate-bad-rules rule-sets)]
    (flatten valid-name-groups)))

(defn solve-1
  "The total error rate for everyone else's tickets"
  [groups]
  (let [{:keys [rules other-tickets]} (parse-input groups)]
    (error-rate rules other-tickets)))

(defn solve-2
  "The product of the values on your ticket whose labels start with
  'departure'"
  [groups]
  (let [{:keys [rules your-ticket other-tickets]} (parse-input groups)
        tickets (good-tickets rules (cons your-ticket other-tickets))
        ordered-rules (order-rules rules tickets)
        your-values (zipmap ordered-rules your-ticket)
        departure-values (filter
                           #(str/starts-with? (first %) "departure")
                           your-values)]
    (reduce * (map second departure-values))))

(assert (= (solve-1 sample-1) 71))

(def input (utils/get-groups 2020 16))

(assert (= (solve-1 input) 25895))
(assert (= (solve-2 input) 5865723727753))

