(ns aoc.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- parse-line [s]
  (let [[[card] [winning-numbers numbers]] (map #(str/split % #"\|") (str/split s #":"))]
    {:card/id (Integer. (second (str/split card #" +")))
     :card/winning-numbers (set (map #(Integer. %) (str/split (str/trim winning-numbers) #" +")))
     :card/numbers (set (map #(Integer. %) (str/split (str/trim numbers) #" +")))}))

(defn- matching-numbers [card]
  (set/intersection (:card/winning-numbers card) (:card/numbers card)))

(defn- collect-cards [all-cards]
  (loop [cards all-cards
         collected-cards 0]
    (let [current-card (first cards)
          new-cards (->> (range (count (matching-numbers current-card)))
                         (map #(+ (:card/id current-card) %))
                         (map #(nth all-cards %)))]
      (if (seq cards)
        (recur (concat new-cards (rest cards)) (inc collected-cards))
        collected-cards))))

(comment
  (with-open [rdr (io/reader "resources/day_4.txt")]
    (->> (line-seq rdr)
         (into [])
         (map parse-line)
         (collect-cards)))
  ;; => 5539496


  ;;
  )

