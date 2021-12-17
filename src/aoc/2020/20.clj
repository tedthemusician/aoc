(ns aoc.2020.20
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

; TODO: Extract text->groups into utils

(def sample
  (utils/lines->groups
    (str/split-lines
      (slurp "./resources/2020/20-sample.txt"))))

(def sea-monster-coords [[18 0]
                         [0 1] [5 1] [6 1] [11 1] [12 1] [17 1] [18 1] [19 1]
                         [1 2] [4 2] [7 2] [10 2] [13 2] [16 2]])

(def sea-monster-width (inc (apply max (map first sea-monster-coords))))
(def sea-monster-height (inc (apply max (map second sea-monster-coords))))

(def flip-v reverse)
(def flip-h (partial map reverse))
(def rotate-cw utils/transpose)
(def rotate-180 (comp flip-v flip-h))
(def rotate-ccw (comp utils/transpose rotate-180))

(def transformers [{:op :0       :func identity}
                   {:op :90      :func rotate-cw}
                   {:op :180     :func rotate-180}
                   {:op :270     :func rotate-ccw}
                   {:op :rev-0   :func flip-h}
                   {:op :rev-90  :func (comp flip-h rotate-cw)}
                   {:op :rev-180 :func flip-v}
                   {:op :rev-270 :func (comp flip-v rotate-cw)}])

(def ops (map :op transformers))
(def op-pairs (combo/selections ops 2))

(defn transformations
  "Every rotation, reflection, and combination thereof that these values can
  be arranged in"
  [matrix]
  (into {} (map (fn [{:keys [op func]}] [op (func matrix)]) transformers)))

; TODO: Extract mapping per character in row to utils
(def parse-row (partial map #(if (= % \#) 1 0)))

(defn parse-tile
  "Parse the ID and every arrangement of a tile"
  [lines]
  (let [[id-str & row-strs] lines
        id (edn/read-string (second (re-matches #"^Tile (\d+):$" id-str)))
        arrangements (transformations (map parse-row row-strs))]
    {:id id, :arrangements arrangements}))

(def parse-tiles (partial map parse-tile))

(defn top-rows-match?
  "In these specific arrangements, are the top rows of these tiles identical?"
  [matrix-1 matrix-2]
  (= (first matrix-1) (first matrix-2)))

(defn matches?
  "Do any arrangements of these tiles match at any edge?"
  [matrix-1 matrix-2]
  (some
    (fn [[op-1 op-2]] (top-rows-match? (op-1 matrix-1) (op-2 matrix-2)))
    op-pairs))

(defn num-matches
  "How many tiles can be placed adjacent to this one?"
  [tiles id]
  (let [groups (group-by #(= id (:id %)) tiles)
        [tile] (get groups true)
        others (get groups false)]
    (count (filter
             (partial matches? (:arrangements tile))
             (map :arrangements others)))))

(defn corner?
  "Is this a corner tile, i.e. do only 2 of its edges fit with another tile?"
  [tiles id]
  (= 2 (num-matches tiles id)))

(defn edge?
  "Is this an edge tile, i.e. do only 3 of its edges fit with another tile?"
  [tiles id]
  (= 3 (num-matches tiles id)))

(defn corner-ids
  "The IDs of the four corner tiles"
  [tiles]
  (filter #(corner? tiles %) (map :id tiles)))

(defn remove-by-id
  "Remove the tile with this ID from a collection of tiles"
  [id tiles]
  (filter #(not= id (:id %)) tiles))

(def arrs (comp vals :arrangements))

(defn make-edge-matcher
  "Make a function that matches a specific edge of one arrangement of a tile
  against a specific edge of all arrangement of annother tile, returning the
  arrangement of the other tile that matches, if any"
  [fixed-tile-func candidate-tile-func]
  (fn [fixed-tile candidate-tile]
    (let [fixed-edge (fixed-tile-func fixed-tile)
          pred #(= fixed-edge (candidate-tile-func %))
          matches (filter pred (arrs candidate-tile))]
      (if (empty? matches)
        nil
        {:arrangement (first matches)
         :id (:id candidate-tile)}))))

(def match-east (make-edge-matcher (partial map last) (partial map first)))
(def match-south (make-edge-matcher (partial last) (partial first)))

(defn find-matching-tile
  "Find an arrangement of a tile in [others] that matches an edge of an
  arrangement of the target tile"
  [matching-func tile others]
  (let [match-results (map (partial matching-func tile) others)]
    (if-let [[match] (filter some? match-results)]
      match)))

(def find-eastern-match (partial find-matching-tile match-east))
(def find-southern-match (partial find-matching-tile match-south))

(defn position-northwest-corner
  "Find an arrangement of a corner tile that allows for a matching tile to its
  east and its south"
  [arrangements others]
  (let [[first-match] (filter
                        #(and (find-eastern-match % others)
                              (find-southern-match % others))
                        arrangements)]
    first-match))

(defn fill-row
  "Given a westernmost tile, find the arrangement of the tile that matches its
  eastern edge. Repeat until no other tiles match, which means we've just
  placed the easternmost tile."
  [western-arrangement others]
  (loop [row [western-arrangement], others others]
    (let [last-tile (last row)
          match (find-eastern-match last-tile others)]
      (if (some? match)
        (let [{:keys [id arrangement]} match]
          (recur (conj row arrangement) (remove-by-id id others)))
        {:row row, :others others}))))

(defn start-next-row
  "Find the tile at the western edge of the row under [prev-row], i.e. the tile
  to the south of the first til in [prev-row]"
  [prev-row others]
  (let [target-tile (first prev-row)]
    (find-southern-match target-tile others)))

(defn fill-next-row
  "Fill the row to the south of prev-row"
  [prev-row others]
    (let [{:keys [id arrangement]} (start-next-row prev-row others)
          others-after-western-tile (remove-by-id id others)]
      (fill-row arrangement others-after-western-tile)))

(defn fill-rows
  "Given the northwestern corner and every other tile, fill the first row, then
  fill every subsequent row until we're out of tiles"
  [northwestern-arrangement others]
  (let [first-row (fill-row northwestern-arrangement others)]
    (loop [rows [(:row first-row)], others (:others first-row)]
      (if (empty? others)
        rows
        (let [prev-row (last rows)
             new-row (fill-next-row prev-row others)]
          (recur (conj rows (:row new-row)) (:others new-row)))))))

(defn arrange-tiles
  "Given a collection of randomly-ordered, randomly-oriented tiles, arrange
  them such that all of their edges match"
  [tiles]
  (let [[corner-id] (corner-ids tiles)
        [corner] (filter #(= corner-id (:id %)) tiles)
        others (remove-by-id corner-id tiles)
        nw-arrangement (position-northwest-corner (arrs corner) others)]
    (fill-rows nw-arrangement others)))

(def strip-ends (comp next butlast))
(def strip-edges (comp (partial map strip-ends) strip-ends))
(def strip-borders (partial map (partial map strip-edges)))

(defn merge-cells
  "Merge the tiles in a tile row into a single list of pixel rows"
  [cells]
  (let [num-rows (count (first cells))]
    (map
      (fn [y] (mapcat #(nth % y) cells))
      (range num-rows))))

(def merge-rows (partial map merge-cells))
(def merge-image (partial apply concat))

(def stitch-image (comp merge-image merge-rows strip-borders arrange-tiles))

(defn get-submatrix
  "Get the submatrix of [matrix] whose (0, 0) is (x, y) of [matrix]"
  [matrix x y]
  (map (partial drop x) (drop y matrix)))

(defn sea-monster-at?
  "Is ([x], [y]) the northwesternmost point of a sea monster in [matrix]?"
  [matrix x y]
  (let [submatrix (get-submatrix matrix x y)]
    (every?
      (fn [[x y]] (= (nth (nth submatrix y) x) 1))
      sea-monster-coords)))

(defn num-sea-monsters
  "How many sea monsters are there in [matrix]?"
  [matrix]
  (let [width (count (first matrix))
        height (count matrix)
        xs (range (inc (- width sea-monster-width)))
        ys (range (inc (- height sea-monster-height)))
        coords (combo/cartesian-product xs ys)]
    (count (filter (partial apply (partial sea-monster-at? matrix)) coords))))

(defn nonzero-sea-monsters
  "Given the fact that only one arrangement of [matrix] has sea monsters, how
  many sea monsters are there in that arrangement of [matrix]?"
  [matrix]
  (let [arrangements (vals (transformations matrix))]
    (first (filter (complement zero?) (map num-sea-monsters arrangements)))))

(defn solve-1
  "The product of all corner tiles' IDs"
  [groups]
  (let [tiles (parse-tiles groups)]
    (reduce * (corner-ids tiles))))

(defn solve-2
  "Given the image that can be stitched from the tiles in [groups], how many #s
  in that image are not some arrangement of sea monsters?"
  [groups]
  (let [tiles (parse-tiles groups)
        image (stitch-image tiles)
        num-ones (reduce + (flatten image))
        num-monsters (nonzero-sea-monsters image)
        num-monster-ones (* num-monsters (count sea-monster-coords))]
    (- num-ones num-monster-ones)))

(defn validate [& args]
  (let [input (utils/get-groups 2020 20)]
    (do
      (assert (= (solve-1 sample) 20899048083289))
      (assert (= (solve-1 input) 13983397496713))
      (assert (= (solve-2 sample) 273))
      (assert (= (solve-2 input) 2424)))))
