(ns aoc.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def PIPE-FILTERS
  {\S (fn [[x y]] [#_[x (dec y)] #_[(dec x) y] [(inc x) y] [x (inc y)]])
   \| (fn [[x y]] [[(dec x) y] [(inc x) y]])
   \- (fn [[x y]] [[x (dec y)] [x (inc y)]])
   \L (fn [[x y]] [[(dec x) y] [x (inc y)]])
   \J (fn [[x y]] [[(dec x) y] [x (dec y)]])
   \7 (fn [[x y]] [[x (dec y)] [(inc x) y]])
   \F (fn [[x y]] [[x (inc y)] [(inc x) y]])})

(def EXAMPLE
  ["FF7FSF7F7F7F7F7F---7"
   "L|LJ||||||||||||F--J"
   "FL-7LJLJ||||||LJL-77"
   "F--JF--7||LJLJ7F7FJ-"
   "L---JF-JLJ.||-FJLJJ7"
   "|F|F-JF---7F7-L7L|7|"
   "|FFJF7L7F-JF7|JL---7"
   "7-L-JL7||F7|L7F-7F7|"
   "L.L7LFJ|||||FJL7||LJ"
   "L7JLJL-JLJLJL--JLJ.L"])

(defn starting-point [lines]
  (loop [x 0]
    (let [y (str/index-of (nth lines x) "S")]
      (if y {:pipe/x x :pipe/y y :pipe/val \S}
          (recur (inc x))))))

(defn next-pipe [lines [previous & _] {:pipe/keys [x y val]}]
  (->>
   ((get PIPE-FILTERS val) [x y])
   (remove #(= % [(:pipe/x previous) (:pipe/y previous)]))
   (map (fn [[x y]] {:pipe/x x :pipe/y y :pipe/val (get-in lines [x y])}))
   (remove (comp nil? :pipe/val))
   (remove (comp #(= % \.) :pipe/val))
   (first)))

(defn walk-pipe [lines starting-pipe]
  (loop [current starting-pipe
         path []]
    (let [next (next-pipe lines path current)]
      (if (= starting-pipe next) (cons current path)
          (recur next (cons current path))))))

(defn crossing? [{:pipe/keys [val]}]
  (let [val (str val)]
    (or (= val "|")
        (= val "S")
        (and (= (first val) \S) (= (last val) \J))
        (and (= (first val) \F) (= (last val) \S))
        (and (= (first val) \F) (= (last val) \J))
        (and (= (first val) \L) (= (last val) \7))
        (and (= (first val) \S) (= (last val) \7))
        (and (= (first val) \L) (= (last val) \S)))))

(defn merge-hits [pipe-1 pipe-2]
  (-> (assoc pipe-1 :pipe/i (:pipe/i pipe-2))
      (update :pipe/val str (:pipe/val pipe-2))))

(defn hits-reducer-fn [path]
  (fn [[last-hit & rest :as hits] hit]
    (if (or (= 1 (abs (- (or (:pipe/i last-hit) 0) (:pipe/i hit))))
            (= (dec (count path)) (abs (- (or (:pipe/i last-hit) 0) (:pipe/i hit)))))
      (cons (merge-hits last-hit hit) rest)
      (cons hit hits))))

(defn in? [path [x y]]
  ;; Exclude path from hits
  (when-not (seq (filter #(and (= (:pipe/x %) x)
                               (= (:pipe/y %) y)) path))
    (let [hits (->> (filter #(= x (:pipe/x %)) path)
                    (filter #(> y (:pipe/y %)))
                    (sort-by :pipe/y)
                    (reduce (hits-reducer-fn path) [])
                    (filter crossing?))]
      (odd? (count hits)))))

(comment

  ;; Check S in the sample, to uncomment the right directions on line 6
  (with-open [rdr (io/reader "resources/day_10.txt")]
    (let [lines (into [] (line-seq rdr))
          path (->> (walk-pipe lines (starting-point lines))
                    (map-indexed (fn [i pipe] (assoc pipe :pipe/i i))))]
      (->>
       (for [x (range (count lines))
             y (range (count (first lines)))]
         (in? path [x y]))
        (filter true?)
        (count))))
  ;; => 501

  ;;
  )
