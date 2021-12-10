(ns aoc.2021.08
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
             "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
             "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
             "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
             "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
             "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
             "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
             "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
             "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
             "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"])

(defn parse-line
  [s]
  (let [[left right] (map #(str/split % #" ") (str/split s #" \| "))]
    {:left left :right right}))

(def simple ((comp :left parse-line) (first (utils/get-lines 2021 8))))

(def digits-by-count {2 :one
                      3 :seven
                      4 :four
                      7 :eight})

(def unambiguous-fives {:13 {:25 5}
                        :25 {:46 6}
                        :46 {:25 2 :13 3}})

(def digits-by-segments {#{0 1 2 4 5 6} 0
                         #{2 5} 1
                         #{0 2 3 4 6} 2
                         #{0 2 3 5 6} 3
                         #{1 2 3 5} 4
                         #{0 1 3 5 6} 5
                         #{0 1 3 4 5 6} 6
                         #{0 2 5} 7
                         #{0 1 2 3 4 5 6} 8
                         #{0 1 2 3 5 6} 9})

(defn group-combos-by-count
  [combos]
  (group-by count (map set combos)))

(defn get-initial-segments
  [groups]
  (let [[one] (groups 2)
        s25 one
        [seven] (groups 3)
        s0 (set/difference seven one)
        [four] (groups 4)
        s13 (set/difference four one)
        [eight] (groups 7)
        s46 (set/difference eight (set/union one seven four))
        letter-sets {:0 s0 :13 s13 :25 s25 :46 s46}]
    (into {} (mapcat (fn [[label letters]] (map #(vector % label) letters)) letter-sets))))

(defn extrapolate-from-five
  [initial combo]
  (let [labels (group-by initial combo)
        [[duped-label unknowns]] (filter #(= 2 (count (second %))) labels)
        known-by-label (unambiguous-fives duped-label)
        known (map #(vector % ((comp known-by-label initial) %)) combo)]
    (filter #(some? (second %)) known)))

(defn has-one-13?
  [labeled-combo]
  (= 1 (count (filter #(= :13 %) labeled-combo))))

(defn get-one-13
  [initial combos]
  (first (filter (comp has-one-13? (partial map initial)) combos)))

(defn find-s1
  [initial combos]
  (let [one-13 (get-one-13 initial combos)
        [s13] (filter #(= :13 (initial %)) one-13)]
    {s13 1}))

(defn get-remaining-segments
  [groups initial]
  (let [s0 {(ffirst (filter #(= :0 (second %)) initial)) 0}
        five-long (groups 5)
        known-from-fives (into {} (mapcat (partial extrapolate-from-five initial) five-long))
        s1 (find-s1 initial (groups 6))
        all-but-s4 (merge s0 s1 known-from-fives)
        s4 {(first (set/difference #{\a \b \c \d \e \f \g} (keys all-but-s4))) 4}]
    (merge all-but-s4 s4)))

(defn get-segments
  [combos]
  (let [groups (group-combos-by-count combos)
        initial (get-initial-segments groups)]
    (get-remaining-segments groups initial)))

(defn translate
  [segments output]
  (digits-by-segments (set (map segments output))))

(defn combine-digits
  [digits]
  (int (reduce-kv (fn [acc index curr]
                (+ acc (* curr (Math/pow 10 index))))
              0
              (vec (reverse digits)))))

(defn get-total
  [line]
  (let [{:keys [left right]} (parse-line line)
        segments (get-segments left)
        values (map (partial translate segments) right)]
    (combine-digits values)))

(defn solve-1
  [lines]
  (let [rights (mapcat (comp :right parse-line) lines)]
    (count (filter #(contains? (set (keys digits-by-count)) (count %)) rights))))

(defn solve-2
  [lines]
  (let [totals (map get-total lines)]
    (reduce + totals)))

(utils/verify-solutions
  [{:method solve-1 :sample 26 :input 521}
   {:method solve-2 :sample 61229 :input 1016804}]
  {:value sample}
  (utils/get-lines 2021 8))
