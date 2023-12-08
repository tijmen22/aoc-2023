(ns aoc.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- gear-position [[[ip prev] [i current] [in next]] current-number position]
  (let [s #(subs % (max 0 (dec position)) (min (count %) (+ 1 position (count current-number))))]
    (cond
      (and (seq prev) (str/index-of (s prev) "*"))
      [ip (str/index-of prev "*" (dec position))]
      (str/index-of (s current) "*")
      [i (str/index-of current "*" (dec position))]
      (and (seq next) (str/index-of (s next) "*"))
      [in (str/index-of next "*" (dec position))])))

(defn- numbers [s]
  (re-seq #"\d+" s))

(defn- check-valid-numbers [[_prev [_i current] _next :as partition]]
  (fn [r number]
    (let [position (str/index-of current number (:last-position r))]
      (-> (assoc r :last-position (+ position (count number)))
          (update :numbers conj {:value (Integer. number)
                                 :adjacent-gear (gear-position partition number position)})))))

(defn- parse-line [[_prev [_i current] _next :as partition]]
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
         (map-indexed (fn [i line] [i line]))
         (partition 3 1)
         (map parse-line)
         (mapcat :numbers)
         (group-by :adjacent-gear)
         (vals)
         (filter #(= 2 (count %)))
         (map (fn [[first second]] (* (:value first) (:value second))))
         (apply +)))
  ;; => 81939900


  ;;
  )
