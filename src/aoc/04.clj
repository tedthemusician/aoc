(ns aoc.04
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc.utils :as utils])
  (:gen-class))

(def mixed-sample ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
             "byr:1937 iyr:2017 cid:147 hgt:183cm"
             ""
             "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
             "hcl:#cfa07d byr:1929"
             ""
             "hcl:#ae17e1 iyr:2013"
             "eyr:2024"
             "ecl:brn pid:760753108 byr:1931"
             "hgt:179cm"
             ""
             "hcl:#cfa07d eyr:2025 pid:166559648"
             "iyr:2011 ecl:brn hgt:59in"])

(def invalid-sample ["eyr:1972 cid:100"
                      "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
                      ""
                      "iyr:2019"
                      "hcl:#602927 eyr:1967 hgt:170cm"
                      "ecl:grn pid:012533040 byr:1946"
                      ""
                      "hcl:dab227 iyr:2012"
                      "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
                      ""
                      "hgt:59cm ecl:zzz"
                      "eyr:2038 hcl:74454a iyr:2023"
                      "pid:3556412378 byr:2007"])

(def valid-sample ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
                    "hcl:#623a2f"
                    ""
                    "eyr:2029 ecl:blu cid:129 byr:1989"
                    "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
                    ""
                    "hcl:#888785"
                    "hgt:164cm byr:2001 iyr:2015 cid:88"
                    "pid:545766238 ecl:hzl"
                    "eyr:2022"
                    ""
                    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" ])

(defn assoc-field [pp s]
  "Associate part of a passport with an existing passport hashmap"
  (let [[field-key field-val] (str/split s #":")]
    (assoc pp (keyword field-key) field-val)))

(defn pp-string->pp [s]
  "Convert a passport string into a passport hashmap"
  (let [field-strings (str/split s #" ")]
    (reduce assoc-field {} field-strings)))

(defn has-all-keys? [pp]
  "Without considering :cid, does this passport have all required keys?"
  (= #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (disj (set (keys pp)) :cid)))

(defn make-num-validator [low high]
  "Create a function to determine whether a string is numeric and is between
  [low] and [high]"
  (fn [s] (let [n (edn/read-string s)]
     (and (number? n) (>= n low) (<= n high)))))

(defn make-re-test
  "Create a function to test whether a string matches a regex exactly"
  [re]
  (fn [s] (some? (re-matches re s))))

(defn hgt-valid? [s]
  "Is this height a number followed immediately by 'cm' or 'in', and does it
  satisfy the low-high requirements of each of those units?"
  (if-let [[full-match height units] (re-matches #"^(\d+)(cm|in)$" s)]
    (case units
      "cm" ((make-num-validator 150 193) height)
      "in" ((make-num-validator 59 76) height))
    false))

(def validators
  {:byr (make-num-validator 1920 2002)
   :iyr (make-num-validator 2010 2020)
   :eyr (make-num-validator 2020 2030)
   :hgt hgt-valid?
   :hcl (make-re-test #"^#[0-9a-f]{6}$")
   :ecl (fn [s] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s))
   :pid (make-re-test #"^\d{9}$")})

(defn field-valid?
  "Does this field meet its value requirements? If the key is unknown, it fails
  by default."
  [[k v]]
  ((get validators k (constantly false)) v))

(defn pp-valid? [pp]
  "Without considering :cid, does this passport have all required fields, and
  do they all meet their value requirements?"
  (let [relevant-fields (dissoc pp :cid)]
    (and (= 7 (count relevant-fields))
         (every? field-valid? relevant-fields))))

(defn solve
  "From an input string, how many passports satisfy [pred]?"
  [pred groups]
  (count (filter pred (map pp-string->pp (utils/groups->paragraphs groups)))))

(def solve-1 (partial solve has-all-keys?))
(def solve-2 (partial solve pp-valid?))

(assert (= (solve-1 (utils/lines->groups mixed-sample)) 2))
(assert (= (solve-2 (utils/lines->groups invalid-sample)) 0))
(assert (= (solve-2 (utils/lines->groups valid-sample)) 4))

(def input (utils/get-line-groups 4))

(assert (= (solve-1 input) 226))
(assert (= (solve-2 input) 160))
