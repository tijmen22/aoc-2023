(ns aoc.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def NUMBERS #{\1 \2 \3 \4 \5 \6 \7 \8 \9})
(def PATTERN #"six|three|two|seven|five|eight|one|nine|four")
(def LETTERS->DIGIT {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                   "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn- number? [char]
  (get NUMBERS char))

(defn- digits [[& chars]]
  (keep number? chars))

(defn- calibration-value [s]
  (let [digits (digits s)]
    (if (= 1 (count digits))
      (Integer. (str (first digits) (first digits)))
      (Integer. (str (first digits) (last digits))))))

(defn- letters-to-digit [s]
  (str/replace s PATTERN (fn [match] (str (get LETTERS->DIGIT match)))))

(comment
  (with-open [rdr (io/reader "resources/day_1.txt")]
    (->> (into [] (line-seq rdr))
         (map letters-to-digit)
         (map calibration-value)
         (apply +)))
  ;; => 52834

  ;;
  )

