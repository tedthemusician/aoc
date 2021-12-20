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
  [[s & ss :as scanners]]
  (if (= 1 (count scanners))
    scanners
    (let [match-groups (group-by (partial get-vantage s) ss)
          nils (get match-groups nil)
          matches (seq (dissoc match-groups nil))
          knowns (reduce merge-match s matches)
          ]
      (concat [knowns] nils))))

(defn solve-1
  [groups]
  (->> groups
       (map parse-group)
       (utils/fix assoc-next-matches)
       first
       count))

(defn solve-2
  [x]
  nil)

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 79}
   #_ {:method solve-2 :sample :s2}]
  {:value sample}
  (utils/get-line-groups 2021 19))
