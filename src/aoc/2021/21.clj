(ns aoc.2021.21
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [4 8])

(defn accum
  [m k v]
  (assoc m k (+ (get m k 0) v)))

(defn wrap
  [maximum n]
  (inc (mod (dec n) maximum)))

(defn make-roll-sets
  ([triplet]
   (lazy-seq (cons triplet (make-roll-sets (map #(wrap 100 (+ 3 %)) triplet)))))
  ([] (make-roll-sets [1 2 3])))

(def roll-sets (make-roll-sets))

(defn init-deterministic-player
  [start]
  {:pos start :score 0 :nrolls 0})

(defn move-deterministic
  [{:keys [pos score nrolls]} triplet]
  (let [sum (reduce + triplet)
        pos' (wrap 10 (+ pos sum))
        score' (+ score pos')]
    {:pos pos' :score score' :nrolls (+ 3 nrolls)}))

(defn won?
  [{:keys [score]}]
  (>= score 1000))

(defn play-deterministic
  [start1 start2 triplets]
  (loop [p1 (init-deterministic-player start1)
         p2 (init-deterministic-player start2)
         p1-roll-sets (take-nth 2 roll-sets)
         p2-roll-sets (take-nth 2 (rest roll-sets))]
    (let [[r1 & rs1] p1-roll-sets
          p1' (move-deterministic p1 r1)]
      (if (won? p1')
        {:p1 p1' :p2 p2}
        (let [[r2 & rs2] p2-roll-sets
              p2' (move-deterministic p2 r2)]
          (if (won? p2')
            {:p1 p1' :p2 p2'}
            (recur p1' p2' rs1 rs2)))))))

(def pos-offset-freqs [{:dp 3 :freq 1}
                       {:dp 4 :freq 3}
                       {:dp 5 :freq 6}
                       {:dp 6 :freq 7}
                       {:dp 7 :freq 6}
                       {:dp 8 :freq 3}
                       {:dp 9 :freq 1}])

(defn init-dirac-game
  [[pos1 pos2]]
  {:pos1 pos1 :score1 0
   :pos2 pos2 :score2 0})

(defn split-one
  [player [game total]]
  (into {}
        (let [pos-key (keyword (str "pos" player))
              score-key (keyword (str "score" player))]
          (map (fn [{:keys [dp freq]}]
                 (let [pos (get game pos-key)
                       score (get game score-key)
                       pos' (wrap 10 (+ pos dp))
                       score' (+ score pos')
                       game' (-> game
                                 (assoc pos-key pos')
                                 (assoc score-key score'))]
                   [game' (* freq total)]))
               pos-offset-freqs))))

(defn split-all
  [player totals]
  (let [ms (map (partial split-one player) totals)]
    (apply merge-with + ms)))

(defn wins-dirac? [n] (>= n 21))
(def p1-wins-dirac? (comp wins-dirac? :score1))
(def p2-wins-dirac? (comp wins-dirac? :score2))

(defn play-dirac
  [[pos1 pos2]]
  (let [game (init-dirac-game [pos1 pos2])]
    (loop [totals {game 1}
           np1wins 0
           np2wins 0
           active-player 1
           n 0]
      (if (empty? totals)
        [np1wins np2wins]
        (let [totals' (split-all active-player totals)
              score-key (keyword (str "score" active-player))
              results (group-by #((comp wins-dirac? score-key) (first %)) totals')
              wins (or (get results true) [])
              others (get results false)
              np1wins' (if (= 1 active-player) (+ np1wins (reduce + (vals wins))) np1wins)
              np2wins' (if (= 2 active-player) (+ np2wins (reduce + (vals wins))) np2wins)
              next-player (- 3 active-player)
              indeterminates (into {} others)]
          (do 
            (recur indeterminates
                   np1wins'
                   np2wins'
                   next-player
                   (inc n))))))))



(defn solve-1
  [[start1 start2]]
  (let [{:keys [p1 p2]} (play-deterministic start1 start2 roll-sets)
        losing-score (:score p2)
        roll-sum (+ (:nrolls p1) (:nrolls p2))]
    (* losing-score roll-sum)))

(defn solve-2
  [starts]
  (apply max (play-dirac starts)))

(utils/verify-solutions
  ; Add an :input key to verify a puzzle input's expected output
  [{:method solve-1 :sample 739785 :input 671580}
   {:method solve-2 :sample 444356092776315 :input 912857726749764}]
  {:value sample}
  [7 6])
