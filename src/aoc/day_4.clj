(ns aoc.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse-line [s]
  (let [[[card] [winning-numbers numbers]] (map #(str/split % #"\|") (str/split s #":"))]
    {:card/id (Integer. (second (str/split card #" +")))
     :card/winning-numbers (set (map #(Integer. %) (str/split (str/trim winning-numbers) #" +")))
     :card/numbers (set (map #(Integer. %) (str/split (str/trim numbers) #" +")))}))

(defn- points [card]
  (->>
   (set/intersection (:card/winning-numbers card) (:card/numbers card))
   (reduce (fn [points _] (if (zero? points) 1 (* 2 points))) 0)))

(comment
  (with-open [rdr (io/reader "resources/day_4.txt")]
    (->> (line-seq rdr)
         (into [])
         (map parse-line)
         (map points)
         (apply +)))
  ;; => 21821


  ;;
  )

