(ns aoc.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-mappings [lines]
  (for [[d s l] (map #(str/split % #" ") lines)
        :let [offset (- (biginteger d) (biginteger s))]]
    {:range [(biginteger s) (dec (+ (biginteger s) (biginteger l)))] :offset offset}))

(defn- parse-map [[header & lines]]
  [(rest (re-find #"(\w+)-to-(\w+)" header))
   (parse-mappings lines)])

(defn- parse-maps [lines]
  (->> (map parse-map lines)
       (into {})))

(defn- find-map [maps from]
  (->> (filter #(= (first %) from) maps)
       (first)))

(defn- intersection [r1 r2]
  (let [start (max (first r1) (first r2))
        end (min (second r1) (second r2))]
    (when (< start end) [start end])))

(defn- difference [r1 r2]
  [(when (< (first r1) (first r2))
     [(first r1) (min (second r1) (first r2))])
   (when (> (second r1) (second r2))
     [(max (first r1) (second r2)) (second r1)])])

(defn- expand-range [r m]
  (let [intersection (intersection r (:range m))
        difference (difference r (:range m))]
    {:affected (remove nil? [(map #(+ (:offset m) %) intersection)])
     :unaffected (remove nil? difference)}))

(defn- expand-ranges [ranges m]
  (->> (map #(expand-range % m) ranges)
       (apply merge-with concat)))

(defn- ranges-reducer [result m]
  (let [expanded-ranges (expand-ranges (:unaffected result) m)]
    (-> (update result :affected concat (:affected expanded-ranges))
        (assoc :unaffected (:unaffected expanded-ranges)))))

(defn- reduce-ranges [maps ranges]
  (let [{:keys [affected unaffected]} (reduce ranges-reducer {:unaffected ranges} maps)]
    (->> (concat affected unaffected)
         (filter seq))))

(defn- corresponding-numbers [lines]
  (let [[[header] & lines] (->> (partition-by #(= "" %) lines)
                                (remove #(= [""] %)))
        seeds (->> (map #(biginteger %) (rest (str/split header #" ")))
                   (partition 2)
                   (map (fn [[start range]] [start (+ start (dec range))])))
        idx (parse-maps lines)]
    (->>
     (reduce-ranges (get idx (find-map (keys idx) "seed")) seeds)
     (reduce-ranges (get idx (find-map (keys idx) "soil")))
     (reduce-ranges (get idx (find-map (keys idx) "fertilizer")))
     (reduce-ranges (get idx (find-map (keys idx) "water")))
     (reduce-ranges (get idx (find-map (keys idx) "light")))
     (reduce-ranges (get idx (find-map (keys idx) "temperature")))
     (reduce-ranges (get idx (find-map (keys idx) "humidity"))))))

(comment
  (time
   (with-open [rdr (io/reader "resources/day_5.txt")]
     (->> (line-seq rdr)
          (into [])
          (corresponding-numbers)
          (apply concat)
          (apply min))))
  ;; => 52510809N

;;
  )
