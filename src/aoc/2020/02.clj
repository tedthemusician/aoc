(ns aoc.2020.02
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["1-3 a: abcde"
             "1-3 b: cdefg"
             "2-9 c: ccccccccc"])

(defn components
  "Get the password, low, high, and interesting letter from an entry"
  [s]
  (let [[amount-str letter-str pw] (str/split s #" ")
        [low high] (map edn/read-string (str/split amount-str #"-"))
        letter (first letter-str)]
    {:pw pw, :low low, :high high, :letter letter}))

(defn valid-1?
  "Does the interesting letter occur in this password from [low] to [high] times?"
  [s]
  (let [{:keys [pw low high letter]} (components s)
        occurrences (get (frequencies pw) letter 0)]
    (and (>= occurrences low) (<= occurrences high))))

(defn valid-2?
  "Does the interesting letter occur at position [low] xor [high] (1-indexed)?"
  [s]
  (let [{:keys [pw low high letter]} (components s)
        match-low? (= letter (nth pw (dec low)))
        match-high? (= letter (nth pw (dec high)))]
    (not= match-low? match-high?)))

(defn solve-1 [lines] (count (filter valid-1? lines)))
(defn solve-2 [lines] (count (filter valid-2? lines)))

(assert (= (solve-1 sample) 2))
(assert (= (solve-2 sample) 1))

(def input (utils/get-lines 2020 2))

(assert (= (solve-1 input) 546))
(assert (= (solve-2 input) 275))

