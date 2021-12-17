(ns aoc.2021.16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def samples ["C200B40A82"
              "04005AC33890"
              "880086C3E88112"
              "CE00C43D881120"
              "D8005AC2A8F0"
              "F600BC2D8F"
              "9C005AC2F8F0"
              "9C0141080250320F1802104A08"])

(defn read-hex-digit
  [d]
  (edn/read-string (str "0x" d)))

(defn get-number-bits
  [x]
  (map #(if (pos? (bit-and x %)) 1 0) [8 4 2 1]))

(defn get-string-bits
  [input]
  (mapcat (comp get-number-bits read-hex-digit) input))

(defn get-value
  [bits]
  (let [value (reduce-kv (fn [acc index curr]
                (+ acc (* curr (Math/pow 2 index))))
              0
              (vec (reverse bits)))]
    (if (> value (Integer/MAX_VALUE))
      (bigint value)
      (int value))))

(def packet-type-ids
  [:sum
   :product
   :minimum
   :maximum
   :literal
   :greater-than
   :less-than
   :equal-to])

(def packet-ops
  {:sum +
   :product *
   :minimum min
   :maximum max
   :literal identity
   :greater-than #(if (> %1 %2) 1 0)
   :less-than #(if (< %1 %2) 1 0)
   :equal-to #(if (= %1 %2) 1 0)})

(def literal-modes
  [:literal-end
   :literal-group])

(def length-modes
  [:subpackets-len
   :subpacket-count])

(defn add-bits
  [next-mode packet bits]
  (-> packet
      (update :content #(concat % bits))
      (assoc :mode next-mode)))

(defn set-packet-len-mode
  [next-mode packet bits]
  (let [len (get-value bits)]
    (-> packet
        (assoc :mode next-mode)
        (assoc :nremaining len))))

(declare init-packet)
(declare consume-all)

(defn consume-pool
  [bits]
  (loop [pool bits, subpackets []]
    (if (empty? pool)
      subpackets
      (let [subpacket (consume-all (init-packet pool))
            remaining-pool (:bits subpacket)]
        (recur remaining-pool (conj subpackets (assoc subpacket :bits (list))))))))

(defn consume-n
  [n bits]
  (loop [nremaining n, available-bits bits, subpackets []]
    (if (zero? nremaining)
      {:subpackets subpackets :leftover available-bits}
      (let [subpacket (consume-all (init-packet available-bits))
            leftover (:bits subpacket)]
        (recur
          (dec nremaining)
          leftover
          (conj subpackets (assoc subpacket :bits (list))))))))

(def modes
  {:version {:len 3
             :func (fn [packet bits]
                     (-> packet
                         (assoc :version (get-value bits))
                         (assoc :mode :packet-type-id)))}
   :packet-type-id {:len 3
                    :func (fn [packet bits]
                            (let [value (get-value bits)
                                  packet-type-id (nth packet-type-ids value)
                                  next-mode (if (= packet-type-id :literal)
                                              :literal-prefix
                                              :length-type-id)]
                              (-> packet
                                  (assoc :type-id packet-type-id)
                                  (assoc :mode next-mode))))}
   ;
   ; LITERALS
   ;
   :literal-prefix {:len 1
                    :func (fn [packet [bit]]
                            (assoc packet :mode (nth literal-modes bit)))}
   :literal-group {:len 4
                   :func (partial add-bits :literal-prefix)}
   :literal-end {:len 4
                 :func (partial add-bits :done)}
   :done {:len 0
          :func (fn [packet bits]
                  (-> packet
                      (update :content get-value)
                      (assoc :mode :fixed)))
          }
   :fixed {:len 0
           :func (fn [packet bits] packet)}
   ;
   ; OPERATIONS
   ;
   :length-type-id {:len 1
                    :func (fn [packet [bit]]
                            (assoc packet :mode (nth length-modes bit)))}
   :subpackets-len {:len 15
                    :func (partial set-packet-len-mode :bit-pool)}
   :subpacket-count {:len 11
                     :func (partial set-packet-len-mode :countdown)}
   ;
   ; NESTING
   ;
   :bit-pool {:len 0
              :func (fn [packet bits]
                      (let [{:keys [bits nremaining]} packet
                            [pool leftover] (split-at nremaining bits)]
                        (-> packet
                            (assoc :bits leftover)
                            (assoc :content (consume-pool pool))
                            (assoc :mode :fixed))))}
   :countdown {:len 0
               :func (fn [packet bits]
                       (let [{:keys [bits nremaining]} packet
                             {:keys [subpackets leftover]} (consume-n nremaining bits)]
                         (-> packet
                             (assoc :bits leftover)
                             (assoc :content subpackets)
                             (assoc :mode :fixed))))}})

(defn init-packet
  [bits]
  {:bits bits
   :content []
   :version nil
   :type-id nil
   :mode :version})

(defn literal?
  [packet]
  (= (:type-id packet) :literal))

(defn consume-next
  [packet n]
  (let [[consumed remaining] (split-at n (:bits packet))]
    {:bits consumed
     :packet (assoc packet :bits remaining)}))

(defn exec-current-mode
  [m]
  (let [{:keys [len func]} (get modes (:mode m))
        {:keys [bits packet]} (consume-next m len)]
    (func packet bits)))

(defn consume-all
  [packet]
  (utils/fix exec-current-mode packet))

(defn strip-packet
  [packet]
  (let [{:keys [content type-id]} packet]
    {:op type-id
     :content (if (literal? packet)
                content
                (map strip-packet content))}))

(defn show-packet
  [packet]
  (let [{:keys [bits content type-id]} packet
        shown-content (if (literal? packet)
                        content
                        (map show-packet content))]
    (-> packet
        (update :bits str/join)
        (assoc :content shown-content))))

(defn sum-version-numbers
  [packet]
  (let [{:keys [version content]} packet
        content-sum (if (literal? packet)
                      0
                      (reduce + (map sum-version-numbers content)))]
    (+ version content-sum)))

(defn evaluate
  [stripped-packet]
  (let [{:keys [op content]} stripped-packet
        func (get packet-ops op)]
    (if (= op :literal)
      content
      (apply func (map evaluate content)))))

(defn solve
  [f text]
  (->> text
       get-string-bits
       init-packet
       consume-all
       f))

(def solve-1 (partial solve sum-version-numbers))

(def solve-2 (partial solve (comp evaluate strip-packet)))

(utils/verify-solutions
  [{:method solve-1 :input 871}
   {:method solve-2 :sample [3 54 7 9 1 0 0 1] :input 68703010504}]
  {:multiple samples}
  (utils/get-text 2021 16))
