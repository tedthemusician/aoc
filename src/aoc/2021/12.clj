(ns aoc.2021.12
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def samples [["start-A"
               "start-b"
               "A-c"
               "A-b"
               "b-d"
               "A-end"
               "b-end"]
              ["dc-end"
               "HN-start"
               "start-kj"
               "dc-start"
               "dc-HN"
               "LN-dc"
               "HN-end"
               "kj-sa"
               "kj-HN"
               "kj-dc"]
              ["fs-end"
               "he-DX"
               "fs-he"
               "start-DX"
               "pj-DX"
               "end-zg"
               "zg-sl"
               "zg-pj"
               "pj-he"
               "RW-he"
               "fs-DX"
               "pj-RW"
               "zg-RW"
               "start-pj"
               "he-WI"
               "zg-he"
               "pj-fs"
               "start-RW"]])

(defn parse-node
  [s]
  (if (#{"start" "end"} s)
    (keyword s)
    s))

(defn parse-line
  [line]
  (map parse-node (str/split line #"-")))

(defn build-graph
  "Create a map of points to their desintations."
  [pairs]
  (let [directional-edges (concat pairs (map reverse pairs))]
    (reduce (fn
              [graph [orig dest]]
              (assoc graph orig (conj (get graph orig []) dest)))
            {}
            directional-edges)))

(defn parse-lines
  "Parse the lines of a file and build the graph they define."
  [lines]
  (build-graph (map parse-line lines)))

(def small (parse-lines (first samples)))
(def med (parse-lines (second samples)))
(def big (parse-lines (last samples)))

(defn available?
  "Is a node available, i.e. is it either :end or uppercase? If allow-lower is
  true, a node can be anything besides :start."
  ([node allow-lower?]
   (or (= :end node)
       (or allow-lower?
           (= node (str/upper-case node)))))
  ([node]
   (available? node false)))

(defn extend-path
  "Extend `path` through `graph` along all edges that connect to :end or an
  uppercase node. `path` should be a vector."
  [graph path]
  (if (= :end (last path))
    [path]
    (let [unavailable-nodes (set (remove available? path))
          orig (last path)
          connections (get graph orig)
          destinations (remove #(contains? unavailable-nodes %) connections)]
      (map #(conj path %) destinations))))

(defn extend-paths
  "Extend all `paths` through `graph` along all edges that connect to :end or an
  uppercase node. `paths` should be a sequence of vectors."
  [graph paths]
  (mapcat (partial extend-path graph) paths))

(defn solve-1
  [lines]
  (let [graph (parse-lines lines)
        paths (utils/fix (partial extend-paths graph) [[:start]])]
    (count paths)))

(solve-1 (first samples))

(defn solve-2
  [x]
  nil)

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample [10 19 226] :input 3485}
   #_ {:method solve-2 :sample [36 103 3509]}]
  {:multiple samples}
  (utils/get-lines 2021 12))
