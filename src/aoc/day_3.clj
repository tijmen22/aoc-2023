(ns aoc.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- valid? [chars]
  (when (re-find #"[^\d.]" chars) true))

(defn- numbers [s]
  (re-seq #"\d+" s))

(defn- adjacent-characters [[prev current next] number position]
  (let [min-x (max 0 (dec position))
        max-x (min (count current) (inc (+ position (count number))))]
    (prn number ": " min-x position max-x)
    (->>
     [(if (seq prev) (subs prev min-x max-x) "")
      (if (seq current) (subs current min-x max-x) "")
      (if (seq next) (subs next min-x max-x) "")]
     (str/join))))

(defn- check-valid-numbers [[_prev current _next :as partition]]
  (fn [r current-number]
    (let [position (str/index-of current current-number (:last-position r))]
      (cond->
       (assoc r :last-position (+ position (count current-number)))
        (valid? (adjacent-characters partition current-number position))
        (update :numbers conj (Integer. current-number))))))

(defn- parse-line [[_prev current _next :as partition]]
  (let [numbers (numbers current)]
    (reduce (check-valid-numbers partition) {:last-position 0} numbers)))

(defn- wrap-empty-lines [seq]
  (->>
   (conj seq "")
   (cons "")))

(comment
  (with-open [rdr (io/reader "resources/day_3.txt")]
    (->> (line-seq rdr)
         (into [])
         (wrap-empty-lines)
         (partition 3 1)
         (map parse-line)
         (mapcat :numbers)
         (apply +)))
  ;; => 537832


  ;;
  )
