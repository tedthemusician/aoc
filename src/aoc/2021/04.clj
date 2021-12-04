(ns aoc.2021.04
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"]
             ["22 13 17 11  0"
             " 8  2 23  4 24"
             "21  9 14 16  7"
             " 6 10  3 18  5"
             " 1 12 20 15 19"]
             [" 3 15  0  2 22"
             " 9 18 13 17  5"
             "19  8  7 25 23"
             "20 11 10 24  4"
             "14 21 16 12  6"]
             ["14 21 17 24  4"
             "10 16 15  9 19"
             "18  8 23 26 20"
             "22 11 13  6  5"
             " 2  0 12  3  7"]])

(defn read-nums
  [sep-re s]
  "Read a string of numbers separated by sep-re"
  (map edn/read-string (str/split (str/trim s) sep-re)))

(defn read-picked
  "Read numbers separated by commas"
  [s]
  (read-nums #"," s))

(defn read-row
  "Read a row of numbers separated by one or more spaces"
  [s]
  (read-nums #"\s+" s))

(def read-board
  "Read the rows of a board"
  (partial map read-row))

(defn read-input
  [groups]
  {:picked (read-picked (ffirst groups))
   :boards (map (comp init-board-state read-board) (rest groups))})

(defn init-board-state
  "Convert a seq of numbers to a seq of maps of numbers and marked states"
  [board]
  (map (fn [nums]
         (map (fn [num] {:value num :marked false})
              nums))
       board))

(defn update-square
  "Mark square iff its value is n"
  [n square]
  (if (= n (:value square))
    (assoc square :marked true)
    square))

(defn update-row
  "Mark any square in row whose value is n"
  [n squares]
  (map (partial update-square n) squares))

(defn update-board
  "Mark any square in board whose value is n"
  [n rows]
  (map (partial update-row n) rows))

(defn update-boards
  "Mark any square in any board in boards whose value is n"
  [n boards]
  (map (partial update-board n) boards))

(defn row-completed?
  "Is every square in this row marked?"
  [row]
  (every? :marked row))

(defn find-completed-row
  "Return the first row in which all squares are marked. Return nil if none
  exists."
  [rows]
  (first (filter row-completed? rows)))

(defn find-completed-line
  "Return the first line--row or column--in which all squares are marked.
  Return nil if none exists."
  [board]
  (let [cols (utils/transpose board)]
    (find-completed-row (concat board cols))))

(defn get-won-boards
  "Return any board that contains a completed line"
  [boards]
  (filter find-completed-line boards))

(defn take-turn
  "Mark n on any board that contains it and return the result of applying
  step to all boards and won boards (if any)."
  [boards n step]
  (let [marked-boards (update-boards n boards)
        won-boards (get-won-boards marked-boards)]
    (step marked-boards won-boards)))

(defn take-greedy-turn
  "If a board is complete after marking, return it wrapped with reduced.
  Otherwise return all boards."
  [boards n]
  (take-turn
    boards
    n
    (fn [boards won-boards]
      (if (not-empty won-boards)
        (reduced {:board (first won-boards) :last-number n})
        boards))))

(defn take-indifferent-turn
  "If only one board is left, return it wrapped with reduced. Otherwise return
  all non-won boards."
  [boards n]
  (take-turn
    boards
    n
    (fn [boards won-boards]
      (if (and (= 1 (count boards)) (= 1 (count won-boards)))
        (reduced {:board (first won-boards) :last-number n})
        (vec (clojure.set/difference (set boards) (set won-boards)))))))

(defn play-until-win
  "Mark picked numbers on boards until one has won, then return that board"
  [picked boards]
  (reduce take-greedy-turn boards picked))

(defn play-all
  "Mark placed numbers on boards and return the last board that wins"
  [picked boards]
  (reduce take-indifferent-turn boards picked))

(defn solve
  "Find a winning board by play-fn and return the product of the last number
  called and the sum of the board's unmarked squares"
  [groups play-fn]
  (let [{:keys [picked boards]} (read-input groups)
        {:keys [board last-number]} (play-fn picked boards)
        unmarked-squares (filter (complement :marked) (flatten board))
        unmarked-nums (map :value unmarked-squares)
        unmarked-sum (reduce + unmarked-nums)]
    (* unmarked-sum last-number)))

(defn solve-1
  [groups]
  (solve groups play-until-win))

(defn solve-2
  [groups]
  (solve groups play-all))

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 4512 :input 74320}
   {:method solve-2 :sample 1924 :input 17884}]
  {:value sample}
  (utils/get-line-groups 2021 04))
