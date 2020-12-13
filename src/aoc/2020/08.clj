(ns aoc.2020.08
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["nop +0"
             "acc +1"
             "jmp +4"
             "acc +3"
             "jmp -3"
             "acc -99"
             "acc +1"
             "jmp -4"
             "acc +6"])

(defn parse-instruction
  "Make a hashmap of an opcode and a value from a string"
  [s]
  (let [[instruction value] (str/split s #" ")]
    {:op (keyword instruction), :value (edn/read-string value)}))

(defn parse-instructions
  "Make a vector of instructions. We need a vector for random access"
  [lines]
  (mapv parse-instruction lines))

(defn swap-op
  "Swap nop with jmp. Leave acc alone."
  [op]
  (case op
    :nop :jmp
    :jmp :nop
    :acc :acc))

(defn swap-op-at
  "Swap the op at position n if it's nop or jmp."
  [ops n]
  (update-in ops [n :op] swap-op))

(defn swap-each
  "Create a set of instruction lists, each of which has one nop or jmp swapped"
  [ops]
  (set (map (partial swap-op-at ops) (range (count ops)))))
 
(defn exec
  "Execute a list of instructions until they loop. Return the value of the
  accumulator immediately before the second execution of any instruction."
  [ops]
  (loop [pos 0 accum 0 history #{}]
    (if-let [{:keys [op value]} (get ops pos)]
      ; We're still within the list of instructions
      (let [next-pos (inc pos)
            new-history (conj history pos)]
        (cond
          ; If we've already executed this instruction, return the accumulator
          ; before executing it again
          (contains? history pos) {:value accum, :did-terminate false}
          ; "acc" -> Add the value to the accumulator and execute the next
          ; instruction
          (= op :acc) (recur next-pos (+ accum value) new-history)
          ; "jmp" -> Move the instruction pointer by the value
          (= op :jmp) (recur (+ pos value) accum new-history)
          ; The only other instruction is "nop", which is a no-op, so just
          ; execute the next instruction
          :else (recur next-pos accum new-history)))
      ; We're past the end of the list of instructions, so the program has
      ; terminated
      {:value accum, :did-terminate true})))

(defn solve-1
  "Execute the input as-is until it loops"
  [lines]
  (:value (exec (parse-instructions lines))))

(defn solve-2
  "Considering all ways to swap one nop or jmp, find a set of instructions that
  terminates. Return its accumulator after termination."
  [lines]
  (let [instructions (parse-instructions lines)
        candidates (swap-each instructions)
        results (map exec candidates)]
    (:value (first (filter :did-terminate results)))))

(assert (= (solve-1 sample) 5))
(assert (= (solve-2 sample) 8))

(def input (utils/get-lines 2020 8))

(assert (= (solve-1 input) 2034))
(assert (= (solve-2 input) 672))

