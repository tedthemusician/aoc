(ns aoc.2022.11
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(def sample [["Monkey 0:"
              "Starting items: 79, 98"
              "Operation: new = old * 19"
              "Test: divisible by 23"
              "If true: throw to monkey 2"
              "If false: throw to monkey 3"]
             ["Monkey 1:"
              "Starting items: 54, 65, 75, 74"
              "Operation: new = old + 6"
              "Test: divisible by 19"
              "If true: throw to monkey 2"
              "If false: throw to monkey 0"]
             ["Monkey 2:"
              "Starting items: 79, 60, 97"
              "Operation: new = old * old"
              "Test: divisible by 13"
              "If true: throw to monkey 1"
              "If false: throw to monkey 3"]
             ["Monkey 3:"
              "Starting items: 74"
              "Operation: new = old + 3"
              "Test: divisible by 17"
              "If true: throw to monkey 0"
              "If false: throw to monkey 1"]])

(def ops
  {"+" +
   "*" *})

(defn parse-starting-items
  "Get the worry level for a monkey's starting items"
  [line]
  (let [numbers (-> line
                    (str/split #": ")
                    second
                    (str/split #", "))]
    {:items (mapv edn/read-string numbers)}))

(defn parse-term
  "Get and parse a numeric term or the key 'old'"
  [s]
  (if (= s "old")
    :old
    (edn/read-string s)))

(defn parse-expression
  "Get the worry level expression associated with a monkey"
  [line]
  (let [[_ t1 op t2] (re-find #"(\w+) (\+|\*) (\w+)$" line)]
    {:expression {:op op
                  :terms (map parse-term [t1 t2])}}))

(defn parse-modulus
  "Get the modulus for a monkey's throw condition"
  [line]
  (let [[_ modulus] (re-matches #"Test: divisible by (\d+)" line)]
    {:modulus (edn/read-string modulus)}))

(defn parse-action
  "Parse a target monkey from an item-throw branch"
  [is-true? map-key line]
  (let [pattern (re-pattern (str "If "
                                 (str is-true?)
                                 ": throw to monkey (\\d+)"))
        [_ index] (re-matches pattern line)]
    (assoc {} map-key (edn/read-string index))))

(def parse-default-action (partial parse-action true :default-target))
(def parse-alternative-action (partial parse-action false :alternative-target))

(def parsers [parse-starting-items
              parse-expression
              parse-modulus
              parse-default-action
              parse-alternative-action])

(defn parse-monkey
  "Parse the index, starting items, update expression, modulus, and actions of
  a monkey"
  [lines]
  (let [maps (map #(%1 %2) parsers (map str/trim (rest lines)))
        monkey (reduce merge maps)]
    (assoc monkey :nticks 0)))

(def s (mapv parse-monkey sample))

(defn apply-op-to-scalar
  "Apply an operation to terms, substituting :old with n"
  [n op terms]
  (let [func (ops op)]
    (apply func (map (fn [term] (if (= :old term) n term)) terms))))

(defn update-scalar-worry-level
  "Update the worry level of the first item in a monkey's list of items"
  [{:keys [items expression] :as monkey}]
  (let [[item] items
        {:keys [op terms]} expression
        new-level (apply-op-to-scalar item op terms)]
    (assoc-in monkey [:items 0] (quot new-level 3))))

(defn test-scalar-worry-level
  "Test whether the worry level of a monkey's first item is divisible by its
  modulus"
  [{:keys [items modulus]}]
  (zero? (mod (first items) modulus)))

(defn throw-next-scalar-item
  "Throw the first item from the monkey at monkey-index to its appropriate
  target"
  [source-index monkeys]
  (let [source-monkey (update-scalar-worry-level (nth monkeys source-index))
        [thrown-item] (:items source-monkey)
        default? (test-scalar-worry-level source-monkey)
        target-key (if default? :default-target :alternative-target)
        target-index (get source-monkey target-key)]
    (-> monkeys
        (update-in [source-index :items] (comp vec rest))
        (update-in [source-index :nticks] inc)
        (update-in [target-index :items] #(conj % thrown-item)))))

(defn take-turn-with-scalars
  "Throw all items from the monkey at monkey-index to their appropriate
  targets"
  [monkeys source-index]
  (let [source-monkey (nth monkeys source-index)
        nitems (count (:items source-monkey))]
    (nth (iterate (partial throw-next-scalar-item source-index) monkeys) nitems)))

(defn play-round-with-scalars
  "All monkeys throw all their items"
  [monkeys]
  (reduce take-turn-with-scalars monkeys (range (count monkeys))))

(defn solve-1
  [input]
  (let [monkeys (mapv parse-monkey input)
        final-state (nth (iterate play-round-with-scalars monkeys) 20)]
    (->> final-state
         (map :nticks)
         sort
         reverse
         (take 2)
         (apply *))))

(defn solve-2
  [input]
  ())

(utils/verify-solutions
  [{:method solve-1 :sample 10605 :input 58786}
   #_ {:method solve-2 :sample nil :input nil}]
  {:value sample}
  (utils/get-groups 2022 11))
