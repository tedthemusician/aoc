(ns aoc.2020.14
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [aoc.utils :as utils])
  (:gen-class))

(def sample-1 ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
               "mem[8] = 11"
               "mem[7] = 101"
               "mem[8] = 0"])

(def sample-2 ["mask = 000000000000000000000000000000X1001X"
                "mem[42] = 100"
                "mask = 00000000000000000000000000000000X0XX"
                "mem[26] = 1"])

(defn parse-line
  "Is this line telling us to alter the bitmask or to store a value?"
  [s]
  (if (str/starts-with? s "mask")
    {:address :mask
     :value (str/replace s "mask = " "")}
    (let [[_ address value] (re-matches #"^mem\[(\d+)\] = (\d+)$" s)]
      {:address (edn/read-string address)
       :value (edn/read-string value)})))

(defn exec-assignment
  "Modify [memory] with one assignment. Apply [modifier-func] to the value of
  [exp] if we're setting the mask. Write to memory with [write-func] otherwise."
  [modifier-func write-func memory exp]
  (let [{:keys [address value]} exp]
    (if (= address :mask)
      (assoc memory :modifier (modifier-func value))
      (write-func memory address value))))

(defn exec-assignments
  "Modify initially-blank memory with a series of assignments"
  [modifier-func write-func assignments]
  (reduce (partial exec-assignment modifier-func write-func) {} assignments))

(defn mem-total
  "What's the sum of all stored values in [memory]?"
  [memory]
  (reduce + (filter number? (vals memory))))

(defn make-single-masker
  "Create a function that applies [op] to the bits of a number in each position
  that [value] appears in [s]"
  [value op s]
  (let [value-str (first (str value))
        full-str (str/join (map #(if (= % value-str) % (- 1 value)) s))
        mask (edn/read-string (str "2r" full-str))]
   (fn [n] (op mask n))))

(defn make-setting-masker
  "Create a function that applies the 0s and 1s in [s] to the bit places of a
  number"
  [s]
  (let [set-0s (make-single-masker 0 bit-and s)
        set-1s (make-single-masker 1 bit-or s)]
    (comp set-0s set-1s)))

(defn get-places [c s]
  "Get the reverse indices of [c] in [s], i.e. their places in a bitmask"
  (utils/indices (partial = c) (reverse (vec s))))

(defn make-updating-masker
  "Make a function that updates the bits of a number that match the reverse
  indices of [c] in [s]. Update those bits with [op]."
  [s c op]
  (let [places (get-places c s)]
    (fn [n] (reduce op n places))))

(defn get-floating-offsets
  "Get every combination of 1 and 0 that arise from toggling the bits in
  [x-places]"
  [x-places]
  (let [place-subsets (combo/subsets x-places)]
    (map (fn [subset] (reduce + (map #(bit-set 0 %) subset))) place-subsets)))

(defn make-floating-masker
  "Create a function that sets 1s from [s] in the bit places of the number,
  then finds all 'floating' values of the number by applying every possible
  combination of 1s and 0s to the bit places of the Xs in [s]"
  [s]
  (let [set-ones (make-updating-masker s \1 bit-set)
        remove-xs (make-updating-masker s \X bit-clear)
        x-places (utils/indices (partial = \X) (reverse (vec s)))]
    (fn [n]
      (let [updated (set-ones (remove-xs n))
            floating-offsets (get-floating-offsets x-places)]
        (map (partial + updated) floating-offsets)))))

(defn write-modified-val
  "Modify [value] according to the setting masker stored in [memory], then
  store that modified value to [address] in [memory]"
  [memory address value]
  (assoc memory address ((:modifier memory) value)))

(defn write-to-modified-addresses
  "Derive the 'floating' addresses from the floating masker stored in [memory]
  applied to [address], then store [value] at each of those addresses"
  [memory address value]
  (let [{:keys [modifier]} memory
        addresses (modifier address)]
    (reduce (fn [mem addr] (assoc mem addr value)) memory addresses)))

(defn make-solver [modifier-func write-func]
  "Make a function that parses assignment strings, applies them to an
  initially-blank memory, and sums the values that are then stored"
  (fn [lines]
    (let [assignments (map parse-line lines)
          memory (exec-assignments modifier-func write-func assignments)]
      (mem-total memory))))

(def solve-1 (make-solver make-setting-masker write-modified-val))

(def solve-2 (make-solver make-floating-masker write-to-modified-addresses))

(assert (= (solve-1 sample-1) 165))
(assert (= (solve-2 sample-2) 208))

(def input (utils/get-lines 2020 14))

(assert (= (solve-1 input) 6513443633260))
(assert (= (solve-2 input) 3442819875191))

