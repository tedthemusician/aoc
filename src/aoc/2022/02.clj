(ns aoc.2021.02
  (:require [aoc.utils :as utils]
            [clojure.set :refer [map-invert]])
  (:gen-class))

(def sample ["A Y"
             "B X"
             "C Z"])

(def codes {\A :rock
            \X :rock
            \B :paper
            \Y :paper
            \C :scissors
            \Z :scissors})

(def responses {\X :lose
                \Y :draw
                \Z :win})

(def wins #{[:paper :rock]
            [:scissors :paper]
            [:rock :scissors]})

(def losing-responses (into {} wins))
(def winning-responses (map-invert losing-responses))

(def move-scores {:rock 1
                  :paper 2
                  :scissors 3})

(defn parse-line-1
  "Convert a line to two plies"
  [line]
  (let [[m1 _ m2] line]
    [(codes m1) (codes m2)]))

(defn parse-line-2
  "Convert a line to a ply and a response"
  [line]
  (let [[m1 _ m2] line]
    [(codes m1) (responses m2)]))

(defn respond
  "Respond to a move as specified by a line"
  [[m r]]
  (case r
    :win (winning-responses m)
    :draw m
    :lose (losing-responses m)))

(defn score-round
  "Calculate a round's score by winner and winning shape"
  [round]
  (let [[m1 m2] round
        [p1-score p2-score] (cond
                              (= m1 m2) [3 3]
                              (contains? wins round) [6 0]
                              :otherwise [0 6])]
    [(+ p1-score (move-scores m1)) (+ p2-score (move-scores m2))]))

(defn solve-1
  [lines]
  (->> lines
       (map (comp second score-round parse-line-1))
       (reduce +)))

(defn solve-2
  [lines]
  (->> lines
       (map (comp
              second
              score-round
              (fn [round] [(first round) (respond round)])
              parse-line-2))
       (reduce +)))

(utils/verify-solutions
  [{:method solve-1 :sample 15 :input 15572}
   {:method solve-2 :sample 12 :input 16098}]
  {:value sample}
  (utils/get-lines 2022 2))
