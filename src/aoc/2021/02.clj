(ns aoc.2021.02
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["forward 5"
             "down 5"
             "forward 8"
             "up 3"
             "down 8"
             "forward 2"])

(defn read-command
  "Read the direction and magnitude of a line"
  [s]
  (let [[dir magnitude-str] (str/split s #" ")
        axis (if (= dir "forward") :z :y)
        sign (if (= dir "up") -1 1)]
    {:axis axis :magnitude (* sign (edn/read-string magnitude-str))}))

(defn sum-distance
  "Get the total distance along an axis"
  [axis commands]
  (reduce + (map :magnitude (filter #(= axis (:axis %)) commands))))

(defn adjust
  [{:keys [y z aim] :as status} {:keys [axis magnitude]}]
  (if (= axis :y)
    (update status :aim (partial + magnitude))
    {:y (+ y (* aim magnitude))
     :z (+ z magnitude)
     :aim aim}))

(defn solve-1
  [s]
  (let [commands (map read-command s)
        y-distance (sum-distance :y commands)
        z-distance (sum-distance :z commands)]
    (* y-distance z-distance)))

(defn solve-2
  [s]
  (let [commands (map read-command s)
        {:keys [y z]}
        (reduce adjust {:y 0 :z 0 :aim 0} commands)]
    (* y z)))

(utils/verify-solutions
  [{:method solve-1 :sample 150 :input 1580000}
   {:method solve-2 :sample 900 :input 1251263225}]
  {:value sample}
  (utils/get-lines 2021 2))
