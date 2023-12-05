(ns aoc.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-set [set]
  (let [[quantity type] (str/split set #" ")]
    {:set/quantity (Integer. quantity)
     :set/type type}))

(defn- parse-line [s]
  (let [[id & sets] (map str/trim (str/split s #",|:|;"))]
    {:game/id (-> (str/split id #" ") (last) (Integer.))
     :game/sets (map parse-set sets)}))

(defn- power [game]
  (->> (group-by :set/type (:game/sets game))
       (vals)
       (map #(map :set/quantity %))
       (map #(apply max %))
       (apply *)))

(comment
  (with-open [rdr (io/reader "resources/day_2.csv")]
    (->> (into [] (line-seq rdr))
         (map parse-line)
         (map power)
         (apply +)))
  ;; => 70924

  ;;
  )
