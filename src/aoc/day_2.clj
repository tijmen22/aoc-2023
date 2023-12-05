(ns aoc.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def RULES {"red"   12
            "green" 13
            "blue"  14})

(defn- parse-set [set]
  (let [[quantity type] (str/split set #" ")]
    {:set/quantity (Integer. quantity)
     :set/type type}))

(defn- parse-line [s]
  (let [[id & sets] (map str/trim (str/split s #",|:|;"))]
    {:game/id (-> (str/split id #" ") (last) (Integer.))
     :game/sets (map parse-set sets)}))

(defn- max-quantity [set]
  (get RULES (:set/type set)))

(defn- possible-set? [set]
  (<= (:set/quantity set) (max-quantity set)))

(defn- possible-game? [game]
  (every? possible-set? (:game/sets game)))

(comment
  (with-open [rdr (io/reader "resources/day_2.csv")]
    (->> (into [] (line-seq rdr))
         (map parse-line)
         (keep #(when (possible-game? %) (:game/id %)))
         (apply +)))
  ;; => 2771

  ;;
  )
