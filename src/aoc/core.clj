(ns aoc.core
  (:require [aoc.utils :as utils]
            [aoc.2021.15 :as day-15])
  (:gen-class))

(defn -main
  "Placeholder"
  [& args]
  (time (utils/show-results
          [{:method day-15/solve-1 :sample 40 :input 717}
           {:method day-15/solve-2 :sample 315}]
          {:value day-15/sample}
          (map day-15/parse-line (utils/get-lines 2021 15)))))

