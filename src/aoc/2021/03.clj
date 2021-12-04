(ns aoc.2021.03
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["00100"
             "11110"
             "10110"
             "10111"
             "10101"
             "01111"
             "00111"
             "11100"
             "10000"
             "11001"
             "00010"
             "01010"])

(defn get-digits
  "Get individual digits of a string as separate strings"
  [s]
  (map edn/read-string (str/split s #"")))

(defn get-digits-by-column
  "Get the digits in each column from a list of equal-length strings of
  binary numbers"
  [lines]
  (utils/transpose (map get-digits lines)))

(defn get-prevalent-digits
  "Get the character, 0 or 1, that occurs most commonly in a list of 0s and 1s.
  If tied, return nil."
  [digits]
  (let [freqs (frequencies digits)
        quantities (map second freqs)
        freqs-by-quantity (sort-by second (seq freqs))
        [value freq] (last freqs-by-quantity)]
    (if (= (first quantities) (second quantities))
      nil
      value)))

(def get-prevalent-digits-by-column
  (comp (partial map get-prevalent-digits)
        get-digits-by-column))

(defn bin-digits-to-dec
  "Convert a list of 0s and 1s to a decimal number"
  [digits]
  (let [places (reverse digits)
        addends (map-indexed (fn [i mul] (* (Math/pow 2 i) mul)) places)]
    (int (reduce + addends))))

(defn invert
  "Convert 0s to 1s and 1s to 0s"
  [digits]
  (map #(- 1 %) digits))

(defn solve-1
  [lines]
  (let [prevalent-digits (get-prevalent-digits-by-column lines)
        inverse-digits (invert prevalent-digits)
        gamma (bin-digits-to-dec prevalent-digits)
        epsilon (bin-digits-to-dec inverse-digits)]
    (* gamma epsilon)))

(defn get-rating
  [nums favor-prevalent?]
  (let [tiebreaker (if favor-prevalent? 1 0)]
    (loop [nums nums index 0]
      (let [columns (utils/transpose nums)
            relevant-column (nth columns index)
            mode (get-prevalent-digits relevant-column)
            target (cond
                     (nil? mode) tiebreaker
                     favor-prevalent? mode
                     :else (- 1 mode))
            matching-nums (filter #(= target (nth % index)) nums)]
        (if (<= (count matching-nums) 1)
          (bin-digits-to-dec (first matching-nums))
          (recur matching-nums (inc index)))))))

(defn solve-2
  [lines]
  (let [nums (map get-digits lines)
        oxygen-rating (get-rating nums true)
        co2-rating (get-rating nums false)]
    (* oxygen-rating co2-rating)))

(utils/verify-solutions
  [{:method solve-1 :sample 198 :input 3148794}
   {:method solve-2 :sample 230 :input 2795310}]
  {:value sample}
  (utils/get-lines 2021 3))
