(ns aoc.utils
  (:require [clojure.string :as str])
  (:gen-class))

(defn get-lines
  [n]
  "Get individual lines from a day's input, left-padded to two digits with 0"
  (let [basename (format "%02d" n)
        fname (str "./resources/" basename ".txt")]
    (str/split-lines (slurp fname))))

