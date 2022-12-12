(ns aoc.2022.11
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.edn :as edn])
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
  (let [[_ op term] (re-find #"(\+|\*) (\w+)$" line)]
    {:expression {:op op
                  :term (parse-term term)}}))

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

(def parse-input (partial mapv parse-monkey))

(defn apply-op-to-scalar
  "Apply an operation to n and term, substituting :old with n"
  [op term n]
  (let [func (ops op)
        term' (if (= :old term) n term)]
    (func term' n)))

(defn update-scalar-worry-level
  "Update the worry level of the first item in a monkey's list of items"
  [{:keys [items expression] :as monkey}]
  (let [[item] items
        {:keys [op term]} expression
        new-level (apply-op-to-scalar op term item)]
    (assoc-in monkey [:items 0] (quot new-level 3))))

(defn test-scalar-worry-level
  "Test whether the worry level of a monkey's first item is divisible by its
  modulus"
  [{:keys [items modulus]}]
  (zero? (mod (first items) modulus)))

(defn convert-scalar-to-remainders
  "Given a list of divisors, convert a scalar into a map from the divisor to
  the remainder of the modulus of the scalar and the divisor"
  [divisors scalar]
  (zipmap divisors
          (map #(mod scalar %) divisors)))

(defn convert-monkey-scalars-to-remainder-maps
  "Convert one monkey's items from scalars to maps of remainders"
  [divisors monkey]
  (update monkey :items (partial mapv
                                 (partial convert-scalar-to-remainders
                                          divisors))))

(defn convert-all-scalars-to-remainder-maps
  "Convert all monkeys' items from scalars to maps of remainders"
  [monkeys]
  (let [divisors (map :modulus monkeys)]
    (mapv (partial convert-monkey-scalars-to-remainder-maps divisors) monkeys)))

(defn apply-op-to-remainder
  "Apply an operation to a term and a remainder"
  [op term [divisor remainder]]
  (let [func (ops op)
        resolved-term (if (= term :old)
                        remainder
                        term)
        remainder' (mod (func remainder resolved-term) divisor)]
    {divisor remainder'}))

(defn apply-op-to-remainders
  "Apply an operation to a term and a remainder"
  [op term remainders]
  (reduce merge (map (partial apply-op-to-remainder op term) remainders)))

(defn update-remainder-worry-level
  "Update the worry level of the first item in a monkey's list of items"
  [{:keys [items expression] :as monkey}]
  (let [[item] items
        {:keys [op term]} expression
        new-level (apply-op-to-remainders op term item)]
    (assoc-in monkey [:items 0] new-level)))

(defn test-remainder-worry-level
  "Test whether the divisor specified by a monkey's modulus is zero."
  [{:keys [items modulus]}]
  (let [remainder (get (first items) modulus)]
    (zero? remainder)))

(defn throw-next-item
  "Throw the first item from the monkey at monkey-index to its appropriate
  target"
  [config source-index monkeys]
  (let [source-monkey ((:update config) (nth monkeys source-index))
        [thrown-item] (:items source-monkey)
        default? ((:test config) source-monkey)
        target-key (if default? :default-target :alternative-target)
        target-index (get source-monkey target-key)]
    (-> monkeys
        (update-in [source-index :items] (comp vec rest))
        (update-in [source-index :nticks] inc)
        (update-in [target-index :items] #(conj % thrown-item)))))

(defn take-turn
  "Throw all items from the monkey at monkey-index to their appropriate
  targets"
  [config monkeys source-index]
  (let [source-monkey (nth monkeys source-index)
        nitems (count (:items source-monkey))]
    (nth (iterate (partial throw-next-item config source-index) monkeys) nitems)))

(defn play-round
  "All monkeys throw all their items"
  [config monkeys]
  (reduce (partial take-turn config) monkeys (range (count monkeys))))

(defn get-monkey-business
  [final-state]
  (->> final-state
       (map :nticks)
       sort
       reverse
       (take 2)
       (apply *)))

(defn solve-1
  [input]
  (let [monkeys (parse-input input)
        play-func (partial play-round {:update update-scalar-worry-level 
                                       :test test-scalar-worry-level})
        final-state (nth (iterate play-func monkeys) 20)]
    (get-monkey-business final-state)))

(defn solve-2
  [input]
  (let [monkeys (convert-all-scalars-to-remainder-maps (parse-input input))
        play-func (partial play-round {:update update-remainder-worry-level
                                       :test test-remainder-worry-level})
        final-state (nth (iterate play-func monkeys) 10000)]
    (get-monkey-business final-state)))

(utils/verify-solutions
  [{:method solve-1 :sample 10605 :input 58786}
   {:method solve-2 :sample 2713310158 :input 14952185856}]
  {:value sample}
  (utils/get-groups 2022 11))
