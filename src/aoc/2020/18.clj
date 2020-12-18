(ns aoc.2020.18
  (:require 
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def samples-1 {"1 + 2 * 3 + 4 * 5 + 6" 71
                "1 + (2 * 3) + (4 * (5 + 6))" 51
                "2 * 3 + (4 * 5)" 26
                "5 + (8 * 3 + 9 + 3 * 4 * 3)" 437
                "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 12240
                "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 13632})

(def samples-2 {"1 + 2 * 3 + 4 * 5 + 6" 231
                "1 + (2 * 3) + (4 * (5 + 6))" 51
                "2 * 3 + (4 * 5)" 46
                "5 + (8 * 3 + 9 + 3 * 4 * 3)" 1445
                "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 669060
                "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 23340})

(defn parse-line
  "Convert a line into a list of terms and operations. edn/read-string will
  automatically convert parenthesized groups into sublists."
  [line]
  (edn/read-string (str "(" line ")")))

(def parse-lines (partial map parse-line))

(defn make-sym-test
  "Create a function that tests whether a parsed symbol matches [string]"
  [string]
  (fn [elem] (and (symbol? elem) (= string (name elem)))))

(def plus (symbol "+"))
(def times (symbol "*"))

(def plus? (make-sym-test "+"))
(def times? (make-sym-test "*"))

(defn parse-op
  "Parse an operator. edn/read-string does this for us, but we have to use
  eval for that."
  [sym]
  (cond
    (plus? sym) +
    (times? sym) *))

(defn make-term-reducer
  "Leave a term unchanged if it's already a number; otherwise reduce it with
  [evaluator]"
  [evaluator]
  (fn [term]
    (if (number? term)
      term
      (evaluator term))))

(declare reduce-term-sequentially)

(defn evaluate-pair
  "Evaluate a binary operation applied to two operands, either of which can be
  a nested operation"
  [sym t1 t2]
  ((parse-op sym) (reduce-term-sequentially t1) (reduce-term-sequentially t2)))

(defn evaluate-sequentially
  "Evaluate the first binary expression in a list, replace the first expression
  with that value, and recur"
  [exp]
  (if (= 1 (count exp))
    (first exp)
    (let [[t1 sym t2 & remaining-items] exp]
      (recur (cons (evaluate-pair sym t1 t2) remaining-items)))))

(def reduce-term-sequentially (make-term-reducer evaluate-sequentially))

(defn extract-singleton
  "If a value is a coll with only one element, return that element. Otherwise
  return the value unchanged."
  [elem]
  (if (and (coll? elem) (= 1 (count elem)))
    (first elem)
    elem))

(defn make-op-splitter
  "Make a function that splits an expression on an operator's symbol"
  [sym sym-test]
  (fn [exp]
    (remove
      (partial = sym)
      (map extract-singleton (partition-by sym-test exp))))) 

(def split-on-plus (make-op-splitter plus plus?))
(def split-on-times (make-op-splitter times times?))

(declare reduce-term-plus-first)

(def evaluate-plus-first
  (make-term-reducer
    (fn [exp]
      (let [factors (split-on-times exp)]
        (reduce * (map reduce-term-plus-first factors))))))

(def reduce-term-plus-first
  (make-term-reducer
    (fn [exp]
      (if (contains? (set exp) times)
        (evaluate-plus-first exp)
        (reduce + (map evaluate-plus-first (split-on-plus exp)))))))

(defn make-solver
  "Make a function that sums the evaulation of each parsed line using
  [evaluator]"
  [evaluator]
  (fn [lines] (reduce + (map evaluator (parse-lines lines)))))

(def solve-1 (make-solver evaluate-sequentially))
(def solve-2 (make-solver evaluate-plus-first))

(defn validate [& args]
  (let [input (utils/get-lines 2020 18)]
    (do
      (assert (every? #(= (solve-1 (vector (first %))) (second %)) samples-1))
      (assert (= (solve-1 input) 12918250417632))
      (assert (every? #(= (solve-2 (vector (first %))) (second %)) samples-2))
      (assert (= (solve-2 input) 171259538712010)))))
