(ns aoc.day-11 
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]))

(def EXAMPLE
  ["...#......"
   ".......#.."
   "#........."
   ".........."
   "......#..."
   ".#........"
   ".........#"
   ".........."
   ".......#.."
   "#...#....."])

(defn- with-index [coll]
  (map-indexed (fn [i x] [i x]) coll))

(defn- galaxies-in-line [[x line]]
  (for [[y char] (with-index line)
        :when (= \# char)]
    [x y]))

(defn- without-galaxy [galaxies n-cols n-rows]
  {:cols (set/difference (set (range n-cols))
                         (set (map second galaxies)))
   :rows (set/difference (set (range n-rows))
                         (set (map first galaxies)))})

(defn- distance [g1 g2 {:keys [rows cols]}]
  (+ (count (filter #(and (> % (min (first g1) (first g2)))
                          (< % (max (first g1) (first g2)))) rows))
     (count (filter #(and (> % (min (second g1) (second g2)))
                          (< % (max (second g1) (second g2)))) cols))
     (abs (- (first g1) (first g2)))
     (abs (- (second g1) (second g2)))))

(comment

  (with-open [rdr (io/reader "resources/day_11.txt")]
    (let [lines (into [] #_EXAMPLE (line-seq rdr))
          galaxies (mapcat galaxies-in-line (with-index lines))
          n-cols (count (first lines))
          n-rows (count lines)
          without-galaxy (without-galaxy galaxies n-cols n-rows)]
      (->>
       (for [g1 galaxies
             g2 galaxies
             :when (not= g1 g2)]
         (sort [g1 g2]))
       (set)
       (map #(distance (first %) (second %) without-galaxy))
       (apply +))))
  ;; => 692506533832
  ;; => 9918828


;;
  )
