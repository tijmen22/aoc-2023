(ns aoc.day-10
  (:require
   [clojure.java.io :as io]
   [clojure.math :as math]
   [clojure.string :as str]))

(def PIPE-FILTERS
  {\S (fn [[x y]] [#_[(dec x) y] [x (dec y)] [x (inc y)] [(inc x) y]]) ;; \J from input doesn't fit
   \| (fn [[x y]] [[(dec x) y] [(inc x) y]])
   \- (fn [[x y]] [[x (dec y)] [x (inc y)]])
   \L (fn [[x y]] [[(dec x) y] [x (inc y)]])
   \J (fn [[x y]] [[(dec x) y] [x (dec y)]])
   \7 (fn [[x y]] [[x (dec y)] [(inc x) y]])
   \F (fn [[x y]] [[(inc x) y] [x (inc y)]])})

(def EXAMPLE
  ["7-F7-"
   ".FJ|7"
   "SJLL7"
   "|F--J"
   "LJ.LJ"])

(defn starting-point [lines]
  (loop [[current & rest] lines
         x 0]
    (if-let [y (str/index-of current "S")] [x y \S]
            (recur rest (inc x)))))

(defn next-pipe [lines previous [x y val]]
  (->>
   ((get PIPE-FILTERS val) [x y])
   (remove #(= % [(first previous) (second previous)]))
   (map (fn [[x y]] [x y (get-in lines [x y])]))
   (remove (comp nil? last))
   (remove (comp #(= % \.) last))))

(defn walk-pipe [lines previous start]
  (loop [[x y val] [(first start) (second start) (last start)]
         previous previous
         i 0]
    (let [next (first (next-pipe lines previous [x y val]))]
      (if (= start next) i
          (recur next [x y val] (inc i))))))

(comment

  (with-open [rdr (io/reader "resources/day_10.txt")]
    (let [lines (into [] (line-seq rdr))
          starting-point (starting-point lines)]
      (->>
       starting-point
       (next-pipe lines nil)
       (first)
       (walk-pipe lines starting-point)
       (* 0.5)
       (math/ceil))))
  ;; => 6856.0

  ;;
  )
