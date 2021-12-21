(ns aoc.2021.20
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [["..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"]
             ["#..#."
              "#...."
              "##..#"
              "..#.."
              "..###"]])

(defn bits->num
  [bits]
  (int (reduce-kv (fn [acc index curr]
                (+ acc (* curr (Math/pow 2 index))))
              0
              (vec (reverse bits)))))

(defn parse-line
  [line]
  (map #(if (= \# %) 1 0) line))

(defn init-image
  [rows default]
  {:rows rows
   :w (count (first rows))
   :h (count rows)
   :default default})

(defn parse-groups
  [[[algo] image]]
  {:algo (vec (parse-line algo))
   :image (init-image (map parse-line image) 0)})

(defn show-line
  [line]
  (apply str (map #(if (zero? %) "." "#") line)))

(defn show-image
  [rows]
  (str/join "\n" (map show-line rows)))

(defn get-pixel
  [image x y]
  (let [{:keys [w h default rows]} image]
    (if (or (< x 0)
            (< y 0)
            (>= x w)
            (>= y h))
      default
      (nth (nth rows y) x))))

(defn get-surrounding-coords
  [x1 y1]
  (let [x0 (dec x1)
        y0 (dec y1)
        x2 (inc x1)
        y2 (inc y1)]
    [[x0 y0] [x1 y0] [x2 y0]
     [x0 y1] [x1 y1] [x2 y1]
     [x0 y2] [x1 y2] [x2 y2]]))

(defn get-surrounding-pixels
  [image x y]
  (map #(apply get-pixel image %) (get-surrounding-coords x y)))

(def get-pixel-value (comp bits->num get-surrounding-pixels))

(defn get-algo-value
  [algo image x y]
  (nth algo (get-pixel-value image x y)))

(def sp (parse-groups sample))
(def spa (:algo sp))
(def spi (:image sp))

(get-algo-value spa spi 2 2)

(defn get-row
  [algo image y]
  (let [{:keys [w]} image]
    (map #(get-algo-value algo image % y) (range -1 (inc w)))))

(defn enhance
  [algo image]
  (let [{:keys [h]} image]
    (map #(get-row algo image %) (range -1 (inc h)))))

(defn iter
  [algo image]
  (let [{:keys [default]} image
        new-default (nth algo (bits->num (repeat 9 default)))]
   (init-image (enhance algo image) new-default)))

(defn get-iter
  [algo image n]
  (:rows (nth (iterate (partial iter algo) image) n)))

(defn solve
  [niters groups]
  (let [{:keys [algo image]} (parse-groups groups)
        rows (get-iter algo image niters)]
    (reduce + (flatten rows))))

(def solve-1 (partial solve 2))

(def solve-2 (partial solve 50))

(utils/verify-solutions
  [#_ {:method solve-1 :sample 35}
   {:method solve-2 :sample 3351}]
  {:value sample}
  (utils/get-line-groups 2021 20))
