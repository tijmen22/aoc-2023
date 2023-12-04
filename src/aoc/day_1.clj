(ns aoc.day-1
  (:require [clojure.java.io :as io]))

(def NUMBERS (set (map identity "1234567890")))

(defn- number? [char]
  (get NUMBERS char))

(defn- digits [[& chars]]
  (keep number? chars))

(defn- calibration-value [s]
  (let [digits (digits s)]
    (if (= 1 (count digits))
      (Integer. (str (first digits) (first digits)))
      (Integer. (str (first digits) (last digits))))))

(comment
  (with-open [rdr (io/reader "resources/day_1.txt")]
    (->> (into [] (line-seq rdr))
         (map calibration-value)
         (apply +)))
  ;; => 53334

  ;;
  )

