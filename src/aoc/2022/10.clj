(ns aoc.2022.10
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:gen-class))

(defn get-resource-lines
  "Get the lines of a plaintext file in resources directory"
  [basename]
  (->> (str  "./resources/2022/" basename ".txt")
       slurp
       str/trim
       str/split-lines))

(def sample (get-resource-lines "10-long-sample"))

(def initial-screen (vec (repeat 6 (vec (repeat 40 0)))))

(defn parse-line
  "Parse a line into a command consisting of an operation and, if the operation
  is `addx`, a numeric argument"
  [line]
  (let [[op & args] (str/split line #" ")
        ]
    (if (= op "noop")
      {:op :noop}
      {:op :addx, :val (edn/read-string (first args))})))

(defn exec-line
  "Push the head of states onto states. If cmd is "
  [states cmd]
  (let [[current-state] states
        new-states (cons current-state states)]
    (if (= :noop (:op cmd))
      new-states
      (let [next-state (+ current-state (:val cmd))]
        (cons next-state new-states)))))

(defn exec-prog
  "Execute a whole program and return it as a vector of states in order of the
  zeroth state, i.e. the initial state, to the final state"
  [cmds]
  (vec (reverse (reduce exec-line (list 1) cmds))))

(defn get-signal-strengths
  "Multiply a state by its index, starting from 1"
  [states]
  (map * states (rest (range))))

(defn get-coord
  "Get the coordinate of matrix associated with n, starting from the upper left
  and moving rightward along columns, then downward along rows"
  [matrix n]
  (let [pixels-per-row (count (first matrix))]
    [(mod n pixels-per-row) (quot n pixels-per-row)]))

(defn get-triplet
  "Get the values below, at, and above a given value"
  [x]
  #{(dec x) x (inc x)})

(defn draw-at
  "Given a set of active x values, either draw a pixel at the coordinate
  associated with n if that coordinate's x value is in the set of active
  x values, or do nothing if it isn't"
  [matrix n xs]
  (let [[x y] (get-coord matrix n)]
    (if (contains? xs x)
      (assoc-in matrix [y x] 1)
      matrix)))

(defn show-row
  "Show a line of the screen, representing blank pixels with . and filled
  pixels with #"
  [row]
  (str/join (map #(if (zero? %) \. \#) row)))

(defn show-matrix
  "Show a screen, representing blank pixels with . and filled pixels with #"
  [matrix]
  (let [lines (map show-row matrix)]
    (str/join "\n" lines)))

(defn draw-screen
  "Compute a list of numbers by applying exec-prog to cmds, then draw those
  pixels to a 40x6 via draw-at."
  [cmds]
  (let [centers (vec (map get-triplet (butlast (exec-prog cmds))))]
    (show-matrix (reduce-kv (fn [matrix index triplet]
                              (draw-at matrix index triplet))
                            initial-screen
                            centers))))

(defn solve-1
  [input]
  (let [signal-strengths (->> input
                              (map parse-line)
                              exec-prog
                              get-signal-strengths)
        relevant-indices [20 60 100 140 180 220]
        relevant-signal-strengths (map #(nth signal-strengths (dec %))
                                       relevant-indices)]
    (reduce + relevant-signal-strengths)))

(defn solve-2
  [input]
  (->> input
       (map parse-line)
       draw-screen))

(utils/verify-solutions
  [{:method solve-1 :sample 13140 :input 13220}
   {:method solve-2
    :sample (str/join "\n" (get-resource-lines "10-part-2-sample-solution"))
    :input (str/join "\n" (get-resource-lines "10-part-2-input-solution"))}]
  {:value sample}
  (utils/get-lines 2022 10))
