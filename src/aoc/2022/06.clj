(ns aoc.2022.06
  (:require [aoc.utils :as utils])
  (:gen-class))

(def samples ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"
              "bvwbjplbgvbhsrlpgdmjqwftvncz"
              "nppdvjthqldpwncqszvftbrmjlhg"
              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
              "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn get-chunks
  "Get all substrings of length len of string s"
  [len s]
  (let [last-index (- (count s) len)]
    (map
      #(subs s % (+ % len))
      (range 0 last-index))))

(defn solve
  [len input]
  (let [chunks (get-chunks len input)
        initial-junk (take-while #(not (apply distinct? %)) chunks)]
    (+ len (count initial-junk))))

(def solve-1 (partial solve 4))
(def solve-2 (partial solve 14))

(utils/verify-solutions
  [{:method solve-1 :sample [7 5 6 10 11] :input 1816}
   {:method solve-2 :sample [19 23 23 29 26]}]
  {:multiple samples}
  (utils/get-text 2022 6))
