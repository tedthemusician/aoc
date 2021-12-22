(ns aoc.2021.22
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["on x=-20..26,y=-36..17,z=-47..7"
             "on x=-20..33,y=-21..23,z=-26..28"
             "on x=-22..28,y=-29..23,z=-38..16"
             "on x=-46..7,y=-6..46,z=-50..-1"
             "on x=-49..1,y=-3..46,z=-24..28"
             "on x=2..47,y=-22..22,z=-23..27"
             "on x=-27..23,y=-28..26,z=-21..29"
             "on x=-39..5,y=-6..47,z=-3..44"
             "on x=-30..21,y=-8..43,z=-13..34"
             "on x=-22..26,y=-27..20,z=-29..19"
             "off x=-48..-32,y=26..41,z=-47..-37"
             "on x=-12..35,y=6..50,z=-50..-2"
             "off x=-48..-32,y=-32..-16,z=-15..-5"
             "on x=-18..26,y=-33..15,z=-7..46"
             "off x=-40..-22,y=-38..-28,z=23..41"
             "on x=-16..35,y=-41..10,z=-47..6"
             "off x=-32..-23,y=11..30,z=-14..3"
             "on x=-49..-5,y=-3..45,z=-29..18"
             "off x=18..30,y=-20..-8,z=-3..13"
             "on x=-41..9,y=-7..43,z=-33..15"
             "on x=-54112..-39298,y=-85059..-49293,z=-27449..7877"
             "on x=967..23432,y=45373..81175,z=27513..53682"])

(defn irange
  [[start end]]
  (range start (inc end)))

(defn parse-line
  [s]
  (let [[_ state & nums]
        (re-matches #"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)" s)
        [x0 x1 y0 y1 z0 z1] (map edn/read-string nums)]
    {:state (if (= state "on") 1 0)
     :xs (sort [x0 x1])
     :ys (sort [y0 y1])
     :zs (sort [z0 z1])}))

(defn oob?
  [[n0 n1]]
  (or (and (< n0 -50) (< n1 -50))
      (and (> n0 50)  (> n1 50))))

(defn any-oob?
  [{:keys [xs ys zs]}]
  (or (oob? xs) (oob? ys) (oob? zs)))

(defn switch-one
  [state cube point]
  (assoc cube point state))

(defn switch-range
  [cube {:keys [state xs ys zs]}]
  (let [xrange (irange xs)
        yrange (irange ys)
        zrange (irange zs)
        points (for [x xrange, y yrange, z zrange] {:x x :y y :z z})]
    (reduce (partial switch-one state) cube points)))

(defn solve-1
  [lines]
  (let [instructions (remove any-oob? (map parse-line lines))
        cube-states (reduce switch-range {} instructions)]
    (reduce + (vals cube-states))))

(defn solve-2
  [x]
  nil)

(utils/verify-solutions
  [{:method solve-1 :sample 474140 :input 607573}
   #_ {:method solve-2 :sample :s2}]
  {:value (str/split-lines (slurp "./resources/2021/22-sample.txt"))}
  (utils/get-lines 2021 22))
