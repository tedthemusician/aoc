(ns aoc.2020.19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample-1 [["0: 4 1 5"
              "1: 2 3 | 3 2"
              "2: 4 4 | 5 5"
              "3: 4 5 | 5 4"
              "4: \"a\""
              "5: \"b\""]
             ["ababbb"
              "bababa"
              "abbbab"
              "aaabbb"
              "aaaabbb"]])

(def sample-2
  (utils/lines->groups
    (str/split-lines
      (slurp "./resources/2020/19-sample-2.txt"))))

; TODO: Extract reading delimited numbers to utils

(defn parse-sequential-targets
  "Parse a list of numbers delimited by spaces as two targets that must be
  matched in succession"
  [s]
  (mapv edn/read-string (str/split s #" ")))

(defn parse-alternative-targets
  "Parse a group of numbers delimited by a pipe as one or more alternative
  targets that may be matched"
  [s]
  (let [sequences (str/split s #" \| ")]
    (mapv parse-sequential-targets sequences)))

(defn parse-target
  "Parse either a group of alternative targets or a character"
  [s]
  (if-let [[_ target-s] (re-matches #"^\"(\w+)\"" s)]
    target-s
    (parse-alternative-targets s)))

(defn parse-rule
  "Parse a target index and its associated rule"
  [line]
  (let [[id-str target-str] (str/split line #": ")
        id (edn/read-string id-str)
        target (parse-target target-str)]
    [id target]))

(defn accum-rule
  "Associate a rule into a hashmap of rules"
  [rules rule]
  (let [[id target] rule]
    (assoc rules id target))) 

(defn parse-rules
  "Parse a list of rules into a hashmap of rules"
  [lines]
  (reduce
    (fn [rules line] (accum-rule rules (parse-rule line)))
    (sorted-map)
    lines))

(defn update-rules
  [rules]
  (-> rules
      (accum-rule [8 [[42] [42 8]]])
      (accum-rule [11 [[42 31] [42 11 31]]])))

(defn char-at
  "The character of [s] at [offset], or nil if [offset] is past the end of [s]"
  [s offset]
  (if (>= offset (count s))
    nil
    (subs s offset (inc offset))))

(declare match-sequential-rules)

(defn match-rule
  "If a rule is a character, match the current set of offsets against that
  character and increment each one that matches. Otherwise get a list of
  offsets that satisfies the targets of this rule."
  [rules target string offsets]
  (let [rule (get rules target)
        unique-offsets (set offsets)]
    (if (string? rule)
      (map inc (filter #(= rule (char-at string %)) unique-offsets))
      (mapcat #(match-sequential-rules rules % string unique-offsets) rule))))

(defn match-sequential-rules
  "Match targets sequentially."
  [rules targets string offsets]
  (reduce (fn [os t] (match-rule rules t string os)) offsets targets))

(defn matches?
  "Does [s] satisfy rule 0 of [rules] exactly, i.e. no trailing characters
  after all matches?"
  [rules s]
  (let [final-indices (match-rule rules 0 s [0])]
    (contains? (set final-indices) (count s))))

(defn count-matches
  "How many strings satisfy rule 0 exactly?"
  [rules strings]
  (count (filter (partial matches? rules) strings)))

(declare append-with-rules)

(defn append-with-rule
  "Append each prefix with each string that follows [target]"
  [rules target prefixes]
  (let [rule (get rules target)]
    (if (string? rule)
    (map #(str % rule) prefixes)
    (mapcat #(append-with-rules rules % prefixes) rule))))

(defn append-with-rules
  "Append each prefix with every string that follows each target in
  succession"
  [rules targets prefixes]
  (reduce (fn [ps t] (append-with-rule rules t ps)) prefixes targets))

(def append-with-rule-memo (memoize append-with-rule))

(defn all-satisfying
  "Every string that satisfies [target]"
  [rules target]
  (set (map seq (append-with-rule rules target [""]))))

(defn get-31-42
  "Every string that satisfies rule 31 and rule 42, which are the two rules
  that can occur in loops in Part 2"
  [rules]
  {31 (all-satisfying rules 31)
   42 (all-satisfying rules 42)})

(defn long-enough?
  "Is it possible to apply our looped rules to this string exactly?"
  [loop-length s]
  (let [str-length (count s)]
    (and (>= str-length (* 3 loop-length)) (= 0 (mod str-length loop-length)))))

(defn satisfies-chain?
  "Does this string contain a valid string from each collection (the strings
  that satisfy rules 42 or 31) in succession?"
  [matchables targets substrs]
  (let [pairs (map vector targets substrs)]
    (every? (fn [[target substr]] (contains? (matchables target) substr)) pairs)))

(defn get-target-chain
  "Get a chain of targets according to how many times rule 11 is nested, e.g.
  42 31, 42 42 31 31, 42 42 42 31 31 31"
  [num-targets n]
  (concat (repeat (- num-targets (* n 2)) 42) (repeat n 42) (repeat n 31)))

(defn get-target-chains
  "Get every possible ordering of 42s followed by 31s that can occur according
  to the nesting dsecribed in Part 2's updated 8 and 11 rules"
  [num-targets]
  (let [last-n (quot (dec num-targets) 2)]
    (map (partial get-target-chain num-targets) (range 1 (inc last-n)))))

(defn matches-with-loops?
  "Does this string match any of the orderings of 42s and 31s possible for its
  length?"
  [matchables loop-length s]
  (when (long-enough? loop-length s)
    (let [chains (get-target-chains (/ (count s) loop-length))
          substrs (partition loop-length s)]
      (some #(satisfies-chain? matchables % substrs) chains))))

(defn get-matches-with-loops
  "Get every string that matches its possible orderings of 42s and 31s"
  [rules strings]
  (let [matchables (get-31-42 rules)
        loop-length (count (first (get matchables 42)))]
    (filter (partial matches-with-loops? matchables loop-length) strings)))

(defn solve-1
  "How many strings satisfy rule 0?"
  [[rules-str strings]]
  (count-matches (parse-rules rules-str) strings))

(defn solve-2
  "How many strings satisfy rule 0 with rules 8 and 11 updated in Part 2?"
  [[rules-str strings]]
  (count (get-matches-with-loops (parse-rules rules-str) strings)))

(defn validate [& args]
  (let [input (utils/get-groups 2020 19)]
    (do
      (assert (= (solve-1 sample-1) 2))
      (assert (= (solve-1 input) 173))
      (assert (= (solve-2 sample-2) 12))
      (assert (= (solve-2 input) 367)))))

