(ns aoc.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def EXAMPLE
  ["32T3K 765"
   "T55J5 684"
   "KK677 28"
   "KTJJT 220"
   "QQQJA 483"])

(def ORDER (str/reverse "AKQT98765432J"))

(defn parse-line [l]
  (str/split l #" "))

(defn high-card [[hand _]]
  (mapv #(str/index-of ORDER %) hand))

(defn safe-sort [coll]
  (if coll (sort coll) [0]))

(defn hand-type [[hand _]]
  (let [freqs (frequencies hand)
        js (get freqs \J)
        freq-vals (-> (dissoc freqs \J) (vals) (safe-sort) (reverse) (vec)
                      (update 0 + (or js 0)))]
    (condp = freq-vals
      [5] 7
      [4 1] 6
      [3 2] 5
      [3 1 1] 4
      [2 2 1] 3
      [2 1 1 1] 2
      [1 1 1 1 1] 1)))

(comment
  (with-open [rdr (io/reader "resources/day_7.txt")]
    (->> (line-seq rdr)
         (into [])
         (map parse-line)
         (sort-by high-card)
         (sort-by hand-type)
         (map-indexed (fn [i [hand bid]]
                        #_(prn i hand (hand-type [hand nil]))
                        (* (inc i) (Integer. bid))))
         (apply +)))
  ;; => 249666369

  ;;
  )
