(ns aoc.2021.19
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :refer [map-invert]]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample (-> "./resources/19-sample.txt"
                slurp
                str/split-lines
                utils/lines->groups))

(defn parse-group
  [group]
  (let [lines (rest group)]
    (map (fn [line] (map edn/read-string (str/split line #","))) lines)))

(let [signs (combo/selections [1 -1] 3)
      directions (map (fn [indices signs]
                        (map (fn [index sign]
                               {:index index :sign sign})
                             indices
                             signs))
                      (repeat [0 1 2]) signs)

      all-povs (mapcat (fn [pov]
                         (take 4
                               (iterate (fn [[x y z]]
                                          [(update y :sign -) x z])
                                        pov)))
                       directions)]
  (def povs (distinct povs)))

(defn move-pov
  [pov coords]
  (map (fn [{:keys [index sign]}] (* sign (nth coords index))) pov))

(defn move-povs
  [pov-index points]
  (map (partial move-pov (nth povs pov-index)) points))

(defn get-offset
  [b1 b2]
  (map - b1 b2))

(defn get-offsets
  [s1 s2]
  (map #(apply get-offset %) (combo/cartesian-product s1 s2)))

(defn get-distance
  [s1 s2]
  (let [freqs (frequencies (get-offsets s1 s2))
        freq-vals (distinct (vals freqs))]
    (if (some #(>= % 12) freq-vals)
      (second (last (sort (map-invert freqs))))
      nil)))

(defn get-vantage
  [s1 s2]
  (let [[match] (keep (fn [index]
                        (if-let [distance (get-distance s1 (move-povs index s2))]
                          {:pov index :distance distance}))
                      (range 24))]
    (or match nil)))

(defn add-distance
  [ds ps]
  (map + ds ps))

(defn merge-match
  [known match]
  (let [[params [points]] match
        {:keys [pov distance]} params
        rotated (move-povs pov points)
        moved (map (partial add-distance distance) rotated)]
    (distinct (concat known moved))))

(defn assoc-next-matches
  [data]
  (let [{:keys [scanners distances]} data]
    (if (= 1 (count scanners))
      data
      (let [[s & ss] scanners
            match-groups (group-by (partial get-vantage s) ss)
            nils (get match-groups nil)
            matches (seq (dissoc match-groups nil))
            new-distances (map (comp :distance first) matches)
            knowns (reduce merge-match s matches)]
        {:distances (concat distances new-distances)
         :scanners (concat [knowns] nils)}))))

(defn get-manhattan-distance
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))
     (Math/abs (- z1 z2))))

(defn get-largest-distance
  [distances]
  (let [pairs (combo/combinations distances 2)
        pair-distances (map (partial apply get-manhattan-distance) pairs)]
    (apply max pair-distances)))

(defn get-all-scanners
  [groups]
  (->> groups
       (assoc {:distances [(list 0 0 0)]} :scanners)
       (utils/fix assoc-next-matches)))

(defn solve-1
  [groups]
  (->> groups
       (map parse-group)
       get-all-scanners
       :scanners
       first
       count))

(defn solve-2
  [groups]
  (->> groups
       (map parse-group)
       get-all-scanners
       :distances
       get-largest-distance))

(utils/verify-solutions
  [{:method solve-1 :sample 79 :input 335}
   {:method solve-2 :sample 3621 :output 10864}]
  {:value sample}
  (utils/get-line-groups 2021 19))
