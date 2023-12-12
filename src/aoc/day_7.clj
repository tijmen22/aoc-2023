(ns aoc.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def EXAMPLE
  ["32T3K 765"
   "T55J5 684"
   "KK677 28"
   "KTJJT 220"
   "QQQJA 483"])

(def ORDER (str/reverse "AKQJT98765432"))

(defn parse-line [l]
  (str/split l #" "))

(defn high-card [[hand _]]
  (mapv #(str/index-of ORDER %) hand))

(defn hand-type [[hand _]]
  (let [freq-vals (->> (frequencies hand) (vals) (sort))]
    (condp = freq-vals
        [5] 7
        [1 4] 6
        [2 3] 5
        [1 1 3] 4
        [1 2 2] 3
        [1 1 1 2] 2
        [1 1 1 1 1] 1)))

(comment
  (with-open [rdr (io/reader "resources/day_7.txt")]
    (->> (line-seq rdr)
         (into [])
         (map parse-line)
         (sort-by high-card)
         (sort-by hand-type)
         (map-indexed (fn [i [hand bid]]
                        (prn i hand (hand-type [hand nil]))
                        (* (inc i) (Integer. bid))))
         (apply +)))
  ;; => 249204891

  ;;
  )
