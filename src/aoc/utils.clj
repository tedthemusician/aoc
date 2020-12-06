(ns aoc.utils
  (:require [clojure.string :as str])
  (:gen-class))

(defn get-lines
  "Get individual lines from a day's input, left-padded to two digits with 0"
  [n]
  (let [basename (format "%02d" n)
        fname (str "./resources/" basename ".txt")]
    (str/split-lines (slurp fname))))

(defn make-paragraphs
  "Get chunks of text separated by blank lines"
  ([lines sep]
   (map #(str/join sep %)
        (remove (partial = [""]) (partition-by empty? lines))))
  ([lines]
   (make-paragraphs lines " ")))

(def get-paragraphs (comp make-paragraphs get-lines))
