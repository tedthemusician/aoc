(ns aoc.2021.15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [[1 1 6 3 7 5 1 7 4 2]
             [1 3 8 1 3 7 3 6 7 2]
             [2 1 3 6 5 1 1 3 2 8]
             [3 6 9 4 9 3 1 5 6 9]
             [7 4 6 3 4 1 7 1 1 1]
             [1 3 1 9 1 2 8 1 3 7]
             [1 3 5 9 9 1 2 4 2 1]
             [3 1 2 5 4 2 1 6 3 9]
             [1 2 9 3 1 3 8 5 2 1]
             [2 3 1 1 9 4 4 5 8 1]])

(def nvisits (atom 0))

(defn getm
  [m {:keys [x y]}]
  (nth (nth m y) x))

(defn assocm
  [m {:keys [x y]} v]
  (let [old-row (nth m y)
        new-row (assoc old-row x v)]
    (assoc m y new-row)))

(defn parse-line
  [lines]
  (map edn/read-string (str/split lines #"")))

(def default-point-data
  {:weight ##Inf
   :value nil
   :prev nil})

(defn init [m mul]
  (let [source (vec (flatten m))
        source-width (count (first m))
        source-height (count m)
        width (* source-width mul)
        height (* source-height mul)]
    {:source source
     :source-width source-width
     :source-height source-height
     :width width
     :height height
     :end {:x (dec width) :y (dec height)}
     :visited #{}
     :to-visit (into (sorted-map) {1 {:x 0 :y 0}})
     ; Add when less than some threshold?
     :known {{:x 0 :y 0} {:weight (first source)
                          :value (first source)
                          :prev nil}}}))

(defn get-neighbors
  [{:keys [x y]}]
  [{:x x :y (dec y)}
   {:x x :y (inc y)}
   {:x (dec x) :y y}
   {:x (inc x) :y y}])

(defn get-bounded-neighbors
  [{:keys [width height]} point]
  (filter #(and (>= (:x %) 0)
                (>= (:y %) 0)
                (< (:x %) width)
                (< (:y %) height))
          (get-neighbors point)))

(defn point->index
  [{:keys [source-width]} {:keys [x y]}]
  (+ x (* source-width y)))

(defn visited?
  [mdata point]
  (contains? (:visited mdata) point))

(defn add-visited
  [mdata point]
  (update-in mdata [:visited] #(conj % point)))

(defn adjust-value
  [x offset]
  (nth (iterate (fn [x'] (if (= x' 9) 1 (inc x'))) x) offset))

(defn get-point-data
  [{:keys [source-width source-height known source]:as mdata}
   {:keys [x y] :as point}]
  (if-let [existing (get-in mdata [:known point])]
    existing
    (let [wrapped-x (mod x source-width)
          wrapped-y (mod y source-height)
          x-level (quot x source-width)
          y-level (quot y source-height)
          source-point {:x wrapped-x :y wrapped-y}
          source-value (nth source (point->index mdata source-point))
          offset (+ x-level y-level)
          adjusted-value (adjust-value source-value offset)]
      (assoc default-point-data :value adjusted-value))))

(defn set-point-data
  [mdata point pdata]
  (assoc-in mdata [:known point] (merge (get-point-data mdata point) pdata)))

(defn probe
  [mdata point pdata neighbor]
  (let [ndata (get-point-data mdata neighbor)
        weight (+ (:value ndata) (:weight pdata))]
    (if (< weight (:weight ndata))
      (set-point-data mdata neighbor {:weight weight
                                      :prev point})
      mdata)))

(defn visit
  [mdata point]
  (let [pdata (get-point-data mdata point)
        neighbors (get-bounded-neighbors mdata point)
        probed (reduce (fn [mdata' neighbor]
                         (probe mdata' point pdata neighbor))
                       mdata
                       neighbors)]
    (add-visited probed point)))

(defn get-lightest-known
  [{:keys [known visited] :as mdata}]
  (let [unvisited-known-points (seq (apply dissoc known visited))
        sorted-points (sort-by (comp :weight second) unvisited-known-points)]
    (ffirst sorted-points)))

(defn visit-lightest-known
  [mdata]
  (do
    (swap! nvisits inc)
    (print "\r")
    (if (zero? (mod @nvisits 1000))
      (print (str (quot @nvisits 1000) "k")))
    (flush)
    (visit mdata (get-lightest-known mdata))))

(defn searching?
  [{:keys [visited end]}]
  (not (contains? visited end)))

(defn visit-all-until-end
  [mdata]
  (let [iterations (iterate visit-lightest-known mdata)]
    (first (drop-while searching? iterations))))

(defn retrace
  [mdata]
  (let [end (:end mdata)]
    (loop [current end, path (list)]
      (let [pdata (get-point-data mdata current)
            path' (conj path pdata)]
        (if-let [prev (:prev pdata)]
          (recur prev path')
          path')))))

(defn solve
  [m mul]
  (let [mdata (init m mul)
        origin-value (:value (get-point-data mdata {:x 0 :y 0}))
        {:keys [known]} (visit-all-until-end mdata)
        destination-weight (:weight (get known (:end mdata)))]
    (println)
    (println "!")
    (swap! nvisits (constantly 0))
    (- destination-weight origin-value)))

(defn solve-1
  [m]
  (solve m 1))

(defn solve-2
  [m]
  (solve m 5))

(utils/verify-solutions
  [{:method solve-1 :sample 40 :input 717}
   #_ {:method solve-2 :sample 315}]
  {:value sample}
  #_ (map parse-line (utils/get-lines 2021 15)))
