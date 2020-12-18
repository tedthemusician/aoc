(ns aoc.2020.05
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["FBFBBFFRLR"
             "BFFFBBFRRR"
             "FFFBBBFRRR"
             "BBFFBBFRLL"])

(defn char->num
  "Convert Fs and Rs to 1s and other chars 0s to parse a larger 'binary' string"
  [c]
  (if (contains? #{\B \R} c) 1 0))

(defn str->num
  "Parse a 'binary' string of Fs and Rs as 1s and other characters as 0s"
  [s]
  (edn/read-string (str "2r" (str/join (map char->num s)))))

(defn get-seat-pos
  "Get a row from the first seven characters of s and a column from the last 3"
  [s]
  (let [[row-str col-str] (split-at 7 s)]
    {:row (str->num row-str)
     :col (str->num col-str)}))

(defn get-seat-id
  "A seat's unique ID is its row multiplied by eight plus its column"
  [s]
  (let [{:keys [row col]} (get-seat-pos s)]
    (+ (* 8 row) col)))

(defn solve-1
  "What's the highest seat ID?"
  [lines]
  (apply max (map get-seat-id lines)))

(defn solve-2
  "The IDs we have will be sequential from their lowest value to their highest
  value, and one value will be missing. Get the difference between the complete
  range of values from lowest to highest and our incomplete set."
  [lines]
  (let [ids (set (map get-seat-id lines))
        low (apply min ids)
        high (apply max ids)
        full-flight (set (range low (inc high)))]
    (first (set/difference full-flight ids))))


(defn validate [& args]
  (let [input (utils/get-lines 2020 5)]
    (do
      (assert (= (solve-1 sample) 820))
      (assert (= (solve-1 input) 894))
      (assert (= (solve-2 input) 579)))))

