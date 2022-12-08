(ns aoc.2022.07
  (:require [aoc.utils :as utils]
            [clojure.set :refer [map-invert]])
  (:gen-class))

(def sample ["$ cd /"
             "$ ls"
             "dir a"
             "14848514 b.txt"
             "8504156 c.dat"
             "dir d"
             "$ cd a"
             "$ ls"
             "dir e"
             "29116 f"
             "2557 g"
             "62596 h.lst"
             "$ cd e"
             "$ ls"
             "584 i"
             "$ cd .."
             "$ cd .."
             "$ cd d"
             "$ ls"
             "4060174 j"
             "8033020 d.log"
             "5626152 d.ext"
             "7214296 k"])


(def initial-state {:fs {"/" {}}
                    :wd (list "/")})

(defn parse-cmd
  "Parse a user-entered command, either 'cd' or 'ls'"
  [word-1 word-2]
  (if (= word-1 "cd")
    {:cmd :cd :target word-2}
    {:cmd :ls}))


(defn parse-listing
  "Parse a file system listing, either a file and its size or a directory"
  [word-1 word-2]
  (if (= word-1 "dir")
    {:type :dir, :name word-2}
    {:type :file, :name word-2, :size (edn/read-string word-1)}))

(defn parse-line
  "Parse a line from terminal scrollback"
  [line]
  (let [[word-1 word-2 word-3] (str/split line #" ")]
    (if (= word-1 "$")
      (merge {:source :user} (parse-cmd word-2 word-3))
      (merge {:source :system} (parse-listing word-1 word-2)))))

(defn cd
  "Change the working directory :wd of state to target"
  [state target]
  (case target
    "/" (assoc state :wd (list "/"))
    ".." (update state :wd rest)
    (update state :wd #(cons target %))))


(defn register-path
  "Add a path to the wording directory :wd of state"
  [state line]
  (let [wd (:wd state)
        name (:name line)
        dir? (= :dir (:type line))
        base-keys (reverse (cons name wd))
        all-keys (cons :fs base-keys)
        contents (if dir? {} (:size line))]
    (assoc-in state all-keys contents)))

(defn execute-line
  "Execute a line from terminal scrollback"
  [state line]
  (cond (= :system (:source line)) (register-path state line)
        (= :cd (:cmd line)) (cd state (:target line))
        :otherwise state))

(defn get-size
  "Get the size of a path, adding subpaths recursively"
  [fs path]
  (let [contents (get-in fs path)]
    (if (number? contents)
      contents
      (let [path-vec (vec path)
            subpaths (map #(conj path-vec %) (keys contents))]
        (reduce + (map (partial get-size fs) subpaths))))))

(defn add-subdirs
  "Add the subdirrectories of a directory in a filesystem"
  [fs dir]
  (let [contents (get-in fs dir)
        ks (keys contents)
        subdir-names (filter #(map? (get contents %)) ks)
        subdirs (map #(conj dir %) subdir-names)]
    (conj (mapcat (partial add-subdirs fs) subdirs) dir)))

(defn build-fs
  "Build a filesystem from a root directory and terminal scrollback"
  [parsed-lines]
  (:fs (reduce execute-line initial-state parsed-lines)))

(defn get-sizes
  "Given a filesystem, get the sizes of all paths"
  [fs]
  (let [paths (add-subdirs fs ["/"])]
    (map (partial get-size fs) paths)))

(defn build-sizes
  "Given terminal scrollback, get the sizes of all paths in a corresponding
  filesystem"
  [input]
  (let [lines (map parse-line input)
        fs (build-fs lines)]
    (get-sizes fs)))

(defn solve-1
  [input]
  (let [sizes (build-sizes input)]
    (reduce + (filter #(<= % 100000) sizes))))

(defn solve-2
  [input]
  (let [sizes (sort (build-sizes input))
        total-usage (last sizes)
        min-size (- total-usage 40000000)
        ]
    (println {:total total-usage :min min-size})
    (first (drop-while #(< % min-size) sizes))))

(utils/verify-solutions
  [{:method solve-1 :sample 95437 :input 1182909}
   {:method solve-2 :sample 24933642 :input 2832508}]
  {:value sample}
  (utils/get-lines 2022 7))
