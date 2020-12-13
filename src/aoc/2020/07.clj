(ns aoc.2020.07
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def sample-1 ["light red bags contain 1 bright white bag, 2 muted yellow bags."
               "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
               "bright white bags contain 1 shiny gold bag."
               "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
               "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
               "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
               "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
               "faded blue bags contain no other bags."
               "dotted black bags contain no other bags."])

(def sample-2 ["shiny gold bags contain 2 dark red bags."
               "dark red bags contain 2 dark orange bags."
               "dark orange bags contain 2 dark yellow bags."
               "dark yellow bags contain 2 dark green bags."
               "dark green bags contain 2 dark blue bags."
               "dark blue bags contain 2 dark violet bags."
               "dark violet bags contain no other bags."])

(defn parse-rule
  "What color and how many?"
  [s]
  (let [[_ quant color] (re-matches #"^(\d+) (.*) bag.*" s)]
    {:color color, :quant (edn/read-string quant)}))

(defn parse-rules
  "What is everything this bag can hold, if anything?"
  [s]
  (if (re-matches #"^no other bags\.$" s)
    []
    (map parse-rule (str/split s #", "))))

(defn parse-bag
  "What color is this bag, and what can it hold?"
  [s]
  (let [[color contents] (str/split s #" bags contain ")]
    {:color color, :holds (parse-rules contents)}))

(defn parse-bags
  "Make a hashmap from color of bag to that bag's attributes. Currently the
  only known attribute is what a bag can hold. Later we will add what the bag
  can be held by."
  [lines]
  (reduce (fn [acc curr]
            (let [{:keys [color holds]} (parse-bag curr)]
              (assoc acc color {:holds holds, :held-by []})))
          {}
          lines))

(defn add-color-held-by
  "Give a hashmap of bags and a specification of what color holds how many of
  what other color, add the holding color and quantity to the held bag's
  :held-by property"
  [bags held-color holding-color quant]
  (update-in bags
             [held-color :held-by]
             conj {:color holding-color :quant quant}))

(defn add-colors-held-by
  "Given a hashmap of bags and a key-value pair of one of the bags, use that
  bag's :holds property to add a :held-by property to the colors it can hold."
  [bags holding-color {:keys [holds]}]
  (reduce (fn [acc curr]
            (add-color-held-by acc (:color curr) holding-color (:quant curr)))
            bags
            holds))

(defn add-all-held-by
  "Populate the :held-by properties of all colors held by another color"
  [bags]
  (reduce-kv add-colors-held-by bags bags))

(def get-bags-info (comp add-all-held-by parse-bags))

(defn get-holders [bags color]
  "How many colors of bags can this color of bag be inside of?"
  (let [{:keys [held-by]} (get bags color)
        direct-holders (map :color held-by)
        indirect-holders (mapcat (partial get-holders bags) direct-holders)]
    (set (concat direct-holders indirect-holders))))

(defn get-num-held
  "How many bags are inside this bag?"
  [bags color]
  (let [{:keys [holds]} (get bags color)]
    (reduce (fn [acc {:keys [quant color]}]
             (+ acc quant (* quant (get-num-held bags color))))
           0
           holds)))

(defn solve-1
  "How many colors of bags can a shiny gold bag be inside of?"
  [lines]
  (count (get-holders (get-bags-info lines) "shiny gold")))

(defn solve-2
  "How many bags are inside one shiny gold bag?"
  [lines]
  (get-num-held (get-bags-info lines) "shiny gold"))

(assert (= (solve-1 sample-1) 4))
(assert (= (solve-2 sample-2) 126))

(def input (utils/get-lines 2020 7))

(assert (= (solve-1 input) 242))
(assert (= (solve-2 input) 176035))

