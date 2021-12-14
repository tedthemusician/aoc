(ns aoc.2021.14
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [["NNCB"]
             ["CH -> B"
              "HH -> N"
              "CB -> H"
              "NH -> C"
              "HB -> C"
              "HC -> B"
              "HN -> C"
              "NN -> C"
              "BH -> H"
              "NC -> B"
              "NB -> B"
              "BN -> B"
              "BB -> N"
              "BC -> B"
              "CC -> N"
              "CN -> C"]])

(defn parse-rule
  [s]
  (let [[pair insertion] (str/split s #" -> ")]
    {:pair (vec pair) :insertion (first (seq insertion))}))

(defn build-rule-map
  [rules]
  (reduce (fn [m {:keys [pair insertion]}]
            (let [[l1 l2] pair]
              (conj m [pair [[l1 insertion] [insertion l2]]])))
          []
          rules))

(defn parse-groups
  [[[template] rules]]
  {:template (seq template)
   :rules (build-rule-map (map parse-rule rules))})

(defn get-initial-freqs
  [cs rules]
  (let [empty-freqs (into {} (map #(vector (first %) 0) rules))
        existing-freqs (frequencies
                (map vec
                     (mapcat (partial partition 2)
                             [cs (rest cs)])))]
    existing-freqs))

(defn get-deltas
  [freqs [old-pair [new-pair-1 new-pair-2]]]
  (if-let [freq (get freqs old-pair)]
    [[old-pair (- freq)]
     [new-pair-1 freq]
     [new-pair-2 freq]]
    nil))

(defn apply-deltas
  [freqs deltas]
  (reduce (fn [fs [pair d]]
            (update fs pair (fnil #(+ % d) 0))) freqs deltas))

(defn apply-rules
  [freqs rules]
  (let [deltas (keep (partial get-deltas freqs) rules)]
    (reduce apply-deltas freqs deltas))) 

(defn get-doubled-sums
  [freqs]
  (let [letters (distinct (flatten (keys freqs)))
        counts (into {} (map #(vector % 0) letters))]
    (reduce-kv (fn [totals [l1 l2] freq]
                 (update
                  (update totals l1 #(+ % freq))
                  l2 #(+ % freq)))
               counts
               freqs)))

(defn solve
  [n groups]
  (let [{:keys [template rules]} (parse-groups groups)
        initial-freqs (get-initial-freqs template rules)
        iterations (iterate #(apply-rules % rules) initial-freqs)
        target-iteration (nth iterations n)
        doubled-sums (vals (get-doubled-sums target-iteration))
        sums (map #(bigint (Math/ceil (/ % 2))) doubled-sums)]
    (- (apply max sums) (apply min sums))))

(def solve-1 (partial solve 10))

(def solve-2 (partial solve 40))

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 1588 :input 2112}
   {:method solve-2 :sample 2188189693529 :input 3243771149914}]
  {:value sample}
  (utils/get-line-groups 2021 14))
