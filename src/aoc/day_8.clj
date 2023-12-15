(ns aoc.day-8
  (:require [clojure.java.io :as io]
            [clojure.math :as math]))

(def EXAMPLE
  ["LR"
   ""
   "11A = (11B, XXX)"
   "11B = (XXX, 11Z)"
   "11Z = (11B, XXX)"
   "22A = (22B, XXX)"
   "22B = (22C, 22C)"
   "22C = (22Z, 22Z)"
   "22Z = (22B, 22B)"
   "XXX = (XXX, XXX)"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

(defn- parse-network [lines]
  (->> lines
       (map #(re-find #"(\w+) = \((\w+), (\w+)\)" %))
       (map rest)
       (map #(zipmap [:from :left :right] %))
       (reduce (fn [idx node] (assoc idx (:from node) (select-keys node [:left :right]))) {})))

(defn- parse-instructions [instructions]
  (map #(get {\L :left \R :right} %) instructions))

(defn- parse-lines [[instructions _ & network]]
  {:instructions (parse-instructions instructions)
   :network (parse-network network)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defn- end-node? [node]
  (= "Z" (subs node 2 3)))

(defn- start-node? [node]
  (= "A" (subs node 2 3)))

(defn- starting-nodes [network]
  (->> (keys network)
       (filter start-node?)))

(defn- next-node [network node instruction]
  (get-in network [node instruction]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduce network

(defn- reduce-instructions [node network instructions]
  (loop [node node
         i 1 ;; i = 1 because all options visit 1 node before the cycle starts
         [now & rest] instructions]
    (let [next-node (next-node network node now)]
      (if (end-node? next-node) i ;; all options reach their target exactly at the end of the cycle
          (recur next-node (inc i) (or rest instructions))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Least common multiple

(defn- trial-division [n]
  (loop [i 2]
    (let [k (math/sqrt n)]
      (if (<= i k)
        (if (zero? (mod n i))
          [i (/ n i)]
          (recur (inc i)))
        :prime))))

(defn- factors [n]
  (loop [numbers [n]
         primes []]
    (if (seq numbers)
      (let [divided (trial-division (first numbers))]
        (if (= :prime divided)
          (recur (rest numbers) (conj primes (first numbers)))
          (recur (concat (rest numbers) divided) primes)))
      primes)))

(defn- least-common-multiple [coll]
  (->> (mapv factors coll)
       (mapv frequencies)
       (apply merge-with max)
       (mapv (fn [[factor max-occurences]] (math/pow factor max-occurences)))
       (apply *)))

(comment
  (with-open [rdr (io/reader "resources/day_8.txt")]
    (let [{:keys [instructions network]} (->> (line-seq rdr) (into []) (parse-lines))]
      (->>
       (map #(reduce-instructions % network instructions) (starting-nodes network))
       ;; => (11653 19241 16531 14363 19783 12737)
       ;; (count instructions) ;; => 271
       ;; (map #(mod % 271) '(11653 19241 16531 14363 19783 12737)) ;; => (0 0 0 0 0 0)
       (least-common-multiple)
       (biginteger))))

  ;; => 9177460370549

;;
  )
