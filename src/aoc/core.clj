(ns aoc.core
  (:require [aoc.utils :as utils]
            [aoc.2020.17 :as day-17])
  (:gen-class))

(defn -main
  "Placeholder"
  [& args]
  (let [sample [".#."
                "..#"
                "###"]
        input (utils/get-lines 2020 17)]
    (println (day-17/solve-2 input))))
