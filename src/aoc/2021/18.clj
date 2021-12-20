(ns aoc.2021.18
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [aoc.utils :as utils])
  (:gen-class))

(def sample [[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
             [[[5,[2,8]],4],[5,[[9,9],0]]]
             [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
             [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
             [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
             [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
             [[[[5,4],[7,7]],8],[[8,3],8]]
             [[9,3],[[9,9],[6,[4,9]]]]
             [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
             [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]])

(defn find-splittable
  ([elem path]
   (cond
     (and (number? elem) (>= elem 10)) {:path path :value elem :type :splittable}
     (number? elem) nil
     :else (let [[left right] elem
                 left-result (find-splittable left (conj path 0))
                 right-result (find-splittable right (conj path 1))]
             (first (remove nil? [left-result right-result])))))
  ([elem]
   (find-splittable elem [])))

(defn find-explodable
  ([elem path]
  (cond (number? elem) nil
        (= 4 (count path)) {:path path :left (first elem) :right (second elem) :type :explodable}
        :else (let [[left right] elem
              left-result (find-explodable left (conj path 0))
              right-result (find-explodable right (conj path 1))]
                (first (remove nil? [left-result right-result])))))
  ([elem]
   (find-explodable elem [])))

(defn path-rank
  [{:keys [path type]}]
  (let [l (conj path (if (= type :explode) 1 0))]
    (int (reduce-kv (fn [acc index curr] (+ acc (* curr (Math/pow 2 index)))) 0 (vec (reverse l))))
    ))

(defn get-neighbor-path
  [dir path]
  (if (every? #(= % dir) path)
    nil
    (let [backwards-path (reverse path)
          [to-swap & kept] (drop-while #(= dir %) backwards-path)
          new-start (reverse (conj kept (- 1 to-swap)))]
      (concat new-start (repeat (- 1 dir))))))

(def get-path-to-left (partial get-neighbor-path 0))
(def get-path-to-right (partial get-neighbor-path 1))

(defn f-at
  [f path snum]
  (loop [traversable-indices []
         remaining-indices path
         elem snum]
    (if (or (empty? remaining-indices) (number? elem))
      (f traversable-indices)
      (let [[i & is] remaining-indices]
        (recur (conj traversable-indices i) is (nth elem i))))))

(defn modify-at [path value snum]
  (f-at (fn [indices] (assoc-in snum indices value)) path snum))

(defn get-at [path snum]
  (f-at (fn [indices] (get-in snum indices)) path snum))

(defn explode-at
  [{:keys [left right path]} snum]
  (let [left-path (get-path-to-left path)
        right-path (get-path-to-right path)
        zeroed-snum (modify-at path 0 snum)
        existing-left (if (some? left-path) (get-at left-path snum))
        existing-right (if (some? right-path) (get-at right-path snum))]
    (cond->> zeroed-snum
      left-path (modify-at left-path (+ existing-left left))

      right-path (modify-at right-path (+ existing-right right)))))

(defn get-split-cells
  [x]
  (let [half (quot x 2)]
    [half (+ half (mod x 2))]))

(defn split-snum-at
  [{:keys [path value]} snum]
  (let [new-cell (get-split-cells value)]
    (modify-at path new-cell snum)))

(defn process-first-reductable
  [snum]
  (let [e (find-explodable snum)
        s (find-splittable snum)]
    (cond
      (some? e) (explode-at e snum)
      (some? s) (split-snum-at s snum)
      :else snum)
    #_ (if (and (nil? s) (nil? e))
      snum
      (let [[r] (sort-by path-rank (remove nil? [s e]))
            {:keys [path]} r]
        (if (= :explodable (:type r))
          (explode-at r snum)
          (split-snum-at r snum))))))

(def reduct (partial utils/fix process-first-reductable))

(defn add
  [s1 s2]
  (reduct [(reduct s1) s2]))

(def sum (partial reduce add))

(defn magnitude
  [[left right]]
  (let [left-full (* 3 (if (number? left) left (magnitude left)))
        right-full (* 2 (if (number? right) right (magnitude right)))]
    (+ left-full right-full)))

(def solve-1 (comp magnitude sum))

(defn solve-2
  [snums]
  (let [pairs (combo/permuted-combinations snums 2)]
    (apply max (map solve-1 pairs))))

(utils/verify-solutions
  [{:method solve-1 :sample 4140}
   {:method solve-2 :sample 3993}]
  {:value sample}
  (map edn/read-string (utils/get-lines 2021 18)))


