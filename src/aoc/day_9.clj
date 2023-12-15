(ns aoc.day-9 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def EXAMPLE
  ["0 3 6 9 12 15"
   "1 3 6 10 15 21"
   "10 13 16 21 30 45"])

(defn- parse-line [l]
  (->> (str/split l #" ") (map #(Integer. %))))

(defn- difference [[a b]]
  (- b a))

(defn- differences [values]
  (map difference (partition 2 1 values)))

(defn- reduce-differences [measurements]
  (loop [values measurements
         prediction 0]
    (let [diffs (differences values)]
      (if (every? zero? diffs) (+ (last measurements) prediction)
          (recur (differences values) (+ prediction (last diffs)))))))

(comment
  (with-open [rdr (io/reader "resources/day_9.txt")]
    (->> (line-seq rdr)
         (into [])
         (map parse-line)
         (map reduce-differences)
         (apply +)))
  ;; => 1696140818

  ;;
  )
