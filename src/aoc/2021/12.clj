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

(defn upper-case?
  "Is a string entirely uppercase?"
  [s]
  (= s (str/upper-case s)))

(defn lower-case?
  "Is a string entirely lowercase?"
  [s]
  (= s (str/lower-case s)))

(defn has-lower-dupe?
  "Does a list of strings contain the same all-lowercase string twice?"
  [path]
  (let [strs (filter string? path)
        lowers (filter lower-case? path)
        freqs (frequencies lowers)]
    (some? (ffirst (filter #(= (second %) 2) freqs)))))

(defn available?
  "Is a node available, i.e. is it either :end or uppercase? If allow-lower is
  true, a node can be anything besides :start."
  ([node allow-lower?]
   (or (= :end node)
       (and (string? node)
            (or allow-lower?
                (upper-case? node)))))
  ([node]
   (available? node false)))

(defn extend-path
  "Extend `path` through `graph` along all edges that satisfy
  `availability-pred`. `path` should be a vector."
  [graph path allow-dupe?]
  (if (= :end (last path))
    [path]
    (let [availability-pred #(available? % (and allow-dupe?
                                               (not (has-lower-dupe? path))))
          unavailable-nodes (set (remove availability-pred path))
          orig (last path)
          connections (get graph orig)
          destinations (remove #(contains? unavailable-nodes %) connections)]
      (map #(conj path %) destinations))))

(defn extend-paths
  "Extend all `paths` through `graph` along all edges that connect to :end or an
  uppercase node. `paths` should be a sequence of vectors."
  [graph paths allow-dupe?]
  (mapcat #(extend-path graph % allow-dupe?) paths))

(defn solve
  [lines allow-dupe?]
  (let [graph (parse-lines lines)
        paths (utils/fix #(extend-paths graph % allow-dupe?) [[:start]])]
    (count paths)))

(def solve-1 #(solve % false))

(def solve-2 #(solve % true))

(utils/verify-solutions
  [{:method solve-1 :sample [10 19 226] :input 3485}
   {:method solve-2 :sample [36 103 3509] :input 85062}]
  {:multiple samples}
  (utils/get-lines 2021 12))
