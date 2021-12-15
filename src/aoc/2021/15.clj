(ns aoc.2021.15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
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

(defn parse-line
  [lines]
  (map edn/read-string (str/split lines #"")))

(defn getm
  [m {:keys [x y]}]
  (nth (nth m y) x))

(defn assocm
  [m {:keys [x y]} v]
  (let [old-row (nth m y)
        new-row (assoc old-row x v)]
    (assoc m y new-row)))

(defn init
  [m]
  (let [blanks (utils/mapm (fn [x]
                             {:value x :prev nil :weight ##Inf})
                           m)
        zero-zero (getm blanks {:x 0 :y 0})
        grid (assocm blanks {:x 0 :y 0} (assoc zero-zero :weight (:value zero-zero)))]
    {:matrix grid
     :width (count (first m))
     :height (count m)
     :visited #{}}))

(def smdata (init sample))
(def sm (:matrix smdata))

(def smalldata (init (take 3 (map (partial take 3) sample))))

(defn get-end
  [{:keys [width height]}]
  {:x (dec width) :y (dec height)})

(defn get-neighbors
  [{:keys [x y]}]
  [{:x x :y (inc y)}
   {:x (inc x) :y y}
   {:x x :y (dec y)}
   {:x (dec x) :y y}])

(defn get-bounded-neighbors
  [{:keys [width height]} point]
  (filter (fn [{:keys [x y]}]
            (and (>= x 0)
                 (>= y 0)
                 (< x width)
                 (< y height)))
          (get-neighbors point)))

(defn explore-neighbor
  [matrix curr neighbor]
  (let [curr-weight (:weight (getm matrix curr))
        neighbor-data (getm matrix neighbor)
        weight-from-curr (+ curr-weight (:value neighbor-data))]
    (if (< weight-from-curr (:weight neighbor-data))
      (let [new-neighbor-data (assoc neighbor-data
                                     :weight weight-from-curr
                                     :prev curr)]
        (assocm matrix neighbor new-neighbor-data))
      matrix)))

(defn explore-neighbors
  [mdata curr]
  (let [neighbors (get-bounded-neighbors mdata curr)
        new-matrix (reduce (fn [m neighbor]
                             (explore-neighbor m curr neighbor))
                           (:matrix mdata)
                           neighbors)]
    (assoc mdata
           :matrix new-matrix
           :visited (conj (:visited mdata) curr))))

(defn get-lightest-node
  [{:keys [matrix width height visited] :as mdata}]
  (loop [y 0, min-weight ##Inf, best {:x 0 :y 0}]
    (if (= y height)
      best
      (let [{:keys [x weight]} (loop [x 0, min-row-weight ##Inf, best-x nil]
                                 (if (= x width)
                                   {:x best-x :weight min-row-weight}
                                   (let [curr {:x x :y y}
                                         {:keys [weight]} (getm matrix curr)]
                                     (if (and
                                           (not (contains? visited curr))
                                           (< weight min-row-weight))
                                       (recur (inc x) weight x)
                                       (recur (inc x) min-row-weight best-x)))))]
        (if (< weight min-weight)
          (recur (inc y) weight {:x x :y y})
          (recur (inc y) min-weight best))))))

(defn iter
  [{:keys [curr mdata]}]
  (let [{:keys [width height]} mdata
        lightest-node (get-lightest-node mdata)
        mdata' (explore-neighbors mdata lightest-node)]
    {:curr lightest-node :mdata mdata'}))

(defn finished?
  [{:keys [matrix] :as mdata}]
  (let [end (get-end mdata)
        {:keys [weight]} (getm matrix end)]
    (< weight ##Inf)))

(defn retrace
  [{:keys [matrix] :as mdata}]
  (let [end (get-end mdata)]
    (loop [curr end, path (list)]
      (let [{:keys [prev]} (getm matrix curr)]
        (if (nil? prev)
          path
          (recur prev (conj path curr)))))))

(defn find-shortest-path
  [m]
  (let [mdata (init m)
        start {:x 0 :y 0}
        iterations (iterate iter {:curr start :mdata mdata})
        finished-iteration (first (drop-while #(not (finished? (:mdata %))) iterations))]
    (retrace (:mdata finished-iteration))))

(defn get-path
  [m]
  (let [end (last (last m))]
    (loop [curr end, points (list end)]
      (if-let [prev (:prev curr)]
        (recur (getm m prev) (conj points prev))
        points))))

(defn solve-1
  [m]
  (let [points (find-shortest-path m)
        values (map (partial getm m) points)]
    (reduce + values)))

(defn solve-2
  [x]
  nil)

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 40}
   #_ {:method solve-2 :sample :s2}]
  {:value sample}
  #_ (map parse-line (utils/get-lines 2021 15)))
