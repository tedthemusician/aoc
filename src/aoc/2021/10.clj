(ns aoc.2021.10
  (:require [aoc.utils :as utils])
  (:gen-class))

(def sample ["[({(<(())[]>[[{[]{<()<>>"
             "[(()[<>])]({[<{<<[]>>("
             "{([(<{}[<>[]}>{[]{[(<()>"
             "(((({<>}<{<{<>}{[]{[]{}"
             "[[<[([]))<([[{}[[()]]]"
             "[{[{({}]{}}([{[{{{}}([]"
             "{<[[]]>}<{[{[{[]{()[[[]"
             "[<(<(<(<{}))><([]([]()"
             "<{([([[(<>()){}]>(<<{{"
             "<{([{{}}[<[[[<>{}]]]>[]]"])

(def matches {\( \)
              \[ \]
              \{ \}
              \< \>})

(def openings (set (keys matches)))

(def corruption-points {\) 3
                        \] 57
                        \} 1197
                        \> 25137})

(def completion-points {\) 1
                        \] 2
                        \} 3
                        \> 4})

(defn get-median
  "Does what it says on the can."
  [xs]
  (nth (sort xs) (int (Math/floor (/ (count xs) 2)))))

(defn ingest-line
  "Find the error in an invalid line If the line ends before all chunks are
  closed, return an :error of :incomplete. Otherwise return an :error of
  :corrupted and a :char of the character that failed to close the open chunk."
  [line]
  (let [[c & cs] line]
    (loop [tail cs, stack (list c)]
      (if (empty? tail)
        {:end (map (partial get matches) stack)}
        (let [[curr & rest-tail] tail
              [top & rest-stack] stack]
          (cond
            (contains? openings curr) (recur rest-tail (conj stack curr))
            (= curr (get matches top)) (recur rest-tail rest-stack)
            :else {:error :corrupted, :char curr}))))))

(defn score-completion
  "Get the ridiculously contrived contest score of a list of closing characters"
  [cs]
  (reduce (fn [total curr]
            (+ (* 5 total)
               (get completion-points curr)))
          0
          cs))

(defn solve-1
  [lines]
  (->> lines
       (keep (comp :char ingest-line))
       (map (partial get corruption-points))
       (reduce +)))

(defn solve-2
  [lines]
  (->> lines
       (keep (comp :end ingest-line))
       (map score-completion)
       sort
       get-median))

(utils/verify-solutions
  [{:method solve-1 :sample 26397 :input 316851}
   {:method solve-2 :sample 288957 :input 2182912364}]
  {:value sample}
  (utils/get-lines 2021 10))

