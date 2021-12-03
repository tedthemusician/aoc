(ns aoc.utils
  (:require [clojure.string :as str]
            [clojure.edn :as edn])
  (:gen-class))

; TODO: Chech which of these are used elsewhere

(defn lines->groups
  "Split lines into groups delimited by blank lines"
  [lines]
  (remove (partial = [""]) (partition-by empty? lines)))

(defn groups->paragraphs
  "Convert groups of lines into single paragraphs"
  ([groups sep]
   (map #(str/join sep %) groups))
  ([groups]
   (groups->paragraphs groups " ")))

(defn get-text
  "Get raw taxt from a day's input"
  [year day]
  (let [basename (format "%02d" day)
        fname (str "./resources/" year "/" basename ".txt")]
    (slurp fname)))

(def get-lines (comp str/split-lines (partial get-text)))

(def get-groups (comp lines->groups get-lines))

(defn get-read-lines
  "Get individual lines from a day's input and parse them as edn"
  [year day]
  (map edn/read-string (get-lines year day)))

(def get-line-groups (comp lines->groups (partial get-lines)))

(defn indices
  "Get the indices of elements in [coll] that satisfy [pred]"
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn parse-nums
  "Parse a comma-separated list of numbers"
  [s]
  (map edn/read-string (str/split s #",")))

(defn transpose
  [m]
  (apply mapv vector m))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn verify-one-input
  [method input expected]
  (let [output (method input)
        result {:input input
                :output (method input)}]
    (if (some? expected)
      (assoc result
             :expected expected
             :success (= output expected))
      result)))

(defn verify-multiple-inputs
  [method inputs expected]
  (cond
    (and (sequential? expected) (= (count inputs) (count expected)))
    (map (partial verify-one-input method) inputs expected)

    (nil? expected)
    (map (partial verify-one-input method) inputs (repeat nil))

    (sequential? expected)
    {:error (str "Received "
                 (count inputs)
                 " inputs but "
                 (count expected)
                 " expected outputs")}

    :else
    {:error "Multiple inputs require multiple expected values"}))

(defn verify-sample
  [method sample expected]
  (let [verification-func (if (sequential? sample) verify-multiple-inputs verify-one-input)]
    (verification-func method sample expected)))

(defn verify-solution
  ([solution sample]
   (let [{:keys [method sample-expected]} solution]
     {:sample (verify-sample method sample sample-expected)}))
  ([solution sample input]
   (let [{:keys [method input-expected]} solution
         results (verify-solution solution sample)]
     (assoc results :puzzle-input
            (when (some? input)
              (verify-one-input method input input-expected))))))

(defn verify-solutions
  [solutions & args]
  (map #(apply (partial verify-solution %) args) solutions))
