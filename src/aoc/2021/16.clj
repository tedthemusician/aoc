(ns aoc.2021.16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [aoc.utils :as utils])
  (:gen-class))

(def diy-samples ["D2FE28"
                  "38006F45291200"
                  "EE00D40C823060"])

(def samples ["8A004A801A8002F478"
              "620080001611562C8802118E34"
              "C0015000016115A2E0802F182340"
              "A0016C880162017C3686B18A3D4780"])

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
  (int (reduce-kv (fn [acc index curr]
                    (+ acc (* curr (Math/pow 2 index))))
                  0
                  (vec (reverse bits)))))

(def packet-type-ids
  {4 :literal})

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
                                  packet-type-id (get packet-type-ids value :unknown)
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
                 :func (partial add-bits :fixed)}
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

(defn show-packet
  [packet]
  (let [{:keys [bits content type-id]} packet
        shown-content (if (literal? packet)
                        (str/join content)
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

(defn solve-1
  [text]
  (->> text
       get-string-bits
       init-packet
       consume-all
       sum-version-numbers))

(defn solve-2
  [x]
  nil)

(utils/verify-solutions
  [{:method solve-1 :sample [16 12 23 31] :input 871}
   #_ {:method solve-2 :sample :s2}]
  {:multiple samples}
  (utils/get-text 2021 16))
