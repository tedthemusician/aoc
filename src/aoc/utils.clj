(ns aoc.utils
  (:require [clojure.string :as str]
            [clojure.edn :as edn])
  (:gen-class))

(defn lines->groups
  "Split lines into groups delimited by blank lines"
  [lines]
  (remove (partial = [""]) (partition-by empty? lines)))

(defn groups->paragraphs
  "Convert groups of lines into single paragraphs"
  ([groups sep]
   (map #(str/join sep %) groups))
  ([groups]
   (groups->paragraphs groups " ")))

(defn get-lines
  "Get individual lines from a day's input, left-padded to two digits with 0"
  [n]
  (let [basename (format "%02d" n)
        fname (str "./resources/" basename ".txt")]
    (str/split-lines (slurp fname))))

(defn get-read-lines
  "Get individual lines from a day's input and parse them as edn"
  [n]
  (map edn/read-string (get-lines n)))

(def get-line-groups (comp lines->groups (partial get-lines)))

