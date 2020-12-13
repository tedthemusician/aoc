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
  [year day]
  (let [basename (format "%02d" day)
        fname (str "./resources/" year "/" basename ".txt")]
    (str/split-lines (slurp fname))))

(defn get-read-lines
  "Get individual lines from a day's input and parse them as edn"
  [year day]
  (map edn/read-string (get-lines year day)))

(def get-line-groups (comp lines->groups (partial get-lines)))

