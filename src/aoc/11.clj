(ns aoc.11
  (:require [clojure.string :as str]
            [aoc.utils :as utils])
  (:gen-class))

(def sample ["L.LL.LL.LL"
             "LLLLLLL.LL"
             "L.L.L..L.."
             "LLLL.LL.LL"
             "L.LL.LL.LL"
             "L.LLLLL.LL"
             "..L.L....."
             "LLLLLLLLLL"
             "L.LLLLLL.L"
             "L.LLLLL.LL"])

(defn sum-numbers
  "Sum a collection of numbers, ignoring non-numeric elements"
  [coll]
  (reduce + (filter number? coll)))

(defn parse-seat
  "Convert a character to 0 or 1 for a filled state or nil if no seat"
  [s]
  (case s
	\. nil
	\L 0
	\# 1))

(defn parse-row
  "Convert a line to a row of seats. Return a vector for random access later."
  [r]
  (vec (map parse-seat r)))

(defn parse-seats
  "Convert lines to rows of seats. Return a vector for random access later."
  [lines]
  (vec (map parse-row lines)))

(defn get-neighbors
  "Get the seats a king's move away from ([x], [y]) in [seats]. Use nil for
  coordinates out of bounds."
  [seats x y]
  (let [prev-row (if (zero? y) (repeat nil) (get seats (dec y)))
        this-row (get seats y)
        next-row (if (= y (count seats)) (repeat nil) (get seats (inc y)))]
    [(get prev-row (dec x))
     (get prev-row x)
     (get prev-row (inc x))
     (get this-row (dec x))
     (get this-row (inc x))
     (get next-row (dec x))
     (get next-row x)
     (get next-row (inc x))]))

(defn past-boundary?
  "Are we past the edge of a matrix?"
  [coll x y]
  (or (neg? x)
      (neg? y)
      (>= x (count (first coll)))
      (>= y (count coll))))

(defn get-first-visible
  "Get the first visible seat of [seats] from ([x], [y]) along a queen's move
  at the heading ([dx], [dy])"
  [seats x y dx dy]
  (loop [x x, y y]
    (let [next-x (+ x dx)
          next-y (+ y dy)]
      (if (past-boundary? seats next-x next-y)
        nil
        (let [seat (get (get seats next-y) next-x)
              hit-top? (and (neg? dy) (zero? next-y))
              hit-bottom? (and (pos? dy) (= (dec (count seats)) next-y))
              hit-left? (and (neg? dx) (zero? next-x))
              hit-right? (and (pos? dx) (= (dec (count (first seats))) next-x))
              did-collide? (or hit-top? hit-bottom? hit-left? hit-right?)]
          (if (or did-collide? (some? seat))
            seat
            (recur next-x next-y)))))))

(defn get-all-visible
  "Get every seat in [seats] visible along a queen's moves from ([x], [y])"
  [seats x y]
  (let [headings [[-1 -1]
                  [0 -1]
                  [1 -1]
                  [-1 0]
                  [1 0]
                  [-1 1]
                  [0 1]
                  [1 1]]]
    (map #(get-first-visible seats x y (first %) (second %)) headings)))

(defn next-state
  "Get a seat's next state by neighbor count and number of filled seats allowed"
  [state neighbors tolerance]
  (let [total (sum-numbers neighbors)]
	(cond
	  (nil? state) nil
	  (zero? total) 1
	  (>= total tolerance) 0
	  :otherwise state)))

(defn iter
  "Get the next state of all seats"
  [seats tabulator tolerance]
  (vec (map-indexed
		 (fn [y row]
		   (vec (map-indexed
				  (fn [x state]
					(next-state state (tabulator seats x y) tolerance))
				  row)))
		 seats)))

(defn stabilize
  "Iterate through seated states until we reach a repeat"
  [seats tabulator tolerance]
  (loop [current seats]
    (let [upcoming (iter current tabulator tolerance)]
      (if (= current upcoming)
        current
        (recur upcoming)))))

(defn get-num-filled
  "Get the number of seats filled after stabilization"
  [seats tabulator tolerance]
  (sum-numbers (flatten (stabilize seats tabulator tolerance))))

(defn solve-1
  "Get the number of seats filled after stabilization using get-neighbors and a
  tolerance of 4"
  [lines]
  (get-num-filled (parse-seats lines) get-neighbors 4))

(defn solve-2
  "Get the number of seats filled after stabilization using get-all-visible and
  a tolerance of 5"
  [lines]
  (get-num-filled (parse-seats lines) get-all-visible 5))

(assert (= (solve-1 sample) 37))
(assert (= (solve-2 sample) 26))

(def input (utils/get-lines 11))

(assert (= (solve-1 input) 2289))
(assert (= (solve-2 input) 2059))

