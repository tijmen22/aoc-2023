(ns aoc.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-mappings [lines]
  (for [[d s l] (map #(str/split % #" ") lines)
        :let [offset (- (biginteger d) (biginteger s))]]
    {:from (biginteger s) :to (+ (biginteger s) (biginteger l)) :offset offset}))

(defn- parse-map [[header & lines]]
  [(rest (re-find #"(\w+)-to-(\w+)" header))
   (parse-mappings lines)])

(defn- parse-maps [lines]
  (->> (map parse-map lines)
       (into {})))

(defn- find-map [maps from]
  (->> (filter #(= (first %) from) maps)
       (first)))

(defn- destination [source m]
  (or (some (fn [{:keys [from to offset]}]
              (when (and (>= source from) (< source to)) (+ source offset))) m)
      source))

(defn- corresponding-numbers [lines]
  (let [[[header] & lines] (->> (partition-by #(= "" %) lines)
                                (remove #(= [""] %)))
        seeds (map #(biginteger %) (rest (str/split header #" ")))
        idx (parse-maps lines)]
    (->>
     (map #(destination % (get idx (find-map (keys idx) "seed"))) seeds)
     (map #(destination % (get idx (find-map (keys idx) "soil"))))
     (map #(destination % (get idx (find-map (keys idx) "fertilizer"))))
     (map #(destination % (get idx (find-map (keys idx) "water"))))
     (map #(destination % (get idx (find-map (keys idx) "light"))))
     (map #(destination % (get idx (find-map (keys idx) "temperature"))))
     (map #(destination % (get idx (find-map (keys idx) "humidity"))))
     (map #(destination % (get idx (find-map (keys idx) "location")))))))

(comment
  (time
   (with-open [rdr (io/reader "resources/day_5.txt")]
     (->> (line-seq rdr)
          (into [])
          (corresponding-numbers)
          (apply min))))
  ;; => 662197086N


  ;;
  )
