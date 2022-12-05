(ns aoc.2022.05
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:gen-class))

(def sample [["    [D]"    
              "[N] [C]"    
              "[Z] [M] [P]"
              " 1   2   3"]
             ["move 1 from 2 to 1"
              "move 3 from 1 to 3"
              "move 2 from 2 to 1"
              "move 1 from 1 to 2"]])

(defn parse-crate
  "Parse a crate in the form '[A]', or nil for three spaces"
  [cs]
  (if (= [\space \space \space] cs)
    nil
    (nth cs 1)))

(defn parse-crate-line
  "Parse a row of crates"
  [max-len s]
  (let [known-crates (map (comp parse-crate
                                (partial take 3))
                          (partition-all 4 s))]
    (take max-len (concat known-crates (repeat nil)))))

(defn parse-stacks
  "Parse rows of crates as stacks"
  [max-len lines]
  (->> lines
       (map (partial parse-crate-line max-len))
       utils/transpose
       (map (partial remove nil?))
       vec))


(defn parse-top
  "Parse rows of crates and their indices as stacks"
  [lines]
  (let [nums (str/split (str/trim (last lines)) #" ")
        len (edn/read-string (last nums))]

    (parse-stacks len (butlast lines))))

(defn parse-instruction
  "Parse an instruction into a quantity, an origin, and a destination"
  [s]
  (let [[_ ncrates origin dest] (re-matches #"move (\d+) from (\d+) to (\d+)" s)]
    {:ncrates (edn/read-string ncrates)
     :origin (edn/read-string origin)
     :dest (edn/read-string dest)}))

(defn parse-input
  "Parse lines grouped as indexed stacks and a list of instructions"
  [groups]
  (let [[top bottom] groups]
    {:stacks (parse-top top)
     :instructions (map parse-instruction bottom)}))

(defn move-crate
  "Move a single crate from origin to dest"
  [stacks origin dest]
  (let [[item] (nth stacks origin)]
    (-> stacks
        (update origin rest)
        (update dest #(cons item %)))))

(defn move-crates-serially
  "Move ncrates one at time from origin to dest"
  [stacks {:keys [ncrates origin dest]}]
  (nth (iterate #(move-crate % (dec origin) (dec dest)) stacks) ncrates))

(defn move-crates-parallel
  "Move a substance of ncrates from origin to dest"
  [stacks {:keys [ncrates origin dest]}]
  (let [source (nth stacks (dec origin))
        [taken remaining] (split-at ncrates source)]
    (-> stacks
        (assoc (dec origin) remaining)
        (update (dec dest) #(concat taken %)))))

(defn execute
  "Execute a parsed input"
  [moving-f {:keys [stacks instructions]}]
  (reduce moving-f stacks instructions))

(defn solve
  [moving-f groups]
  (->> groups
       parse-input
       (execute moving-f)
       (map first)
       (apply str)))

(def solve-1 (partial solve move-crates-serially))
(def solve-2 (partial solve move-crates-parallel))

(utils/verify-solutions
  [{:method solve-1 :sample "CMZ" :input "VGBBJCRMN"}
   {:method solve-2 :sample "MCD" :input "LBBVJBRMH"}]
  {:value sample}
  (utils/get-groups 2022 5))
