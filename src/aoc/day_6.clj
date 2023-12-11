(ns aoc.day-6 
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-lines [[time distance]]
  [(->> (str/split time #":") (last) (re-seq #"\d+") (str/join) (biginteger))
   (->> (str/split distance #":") (last) (re-seq #"\d+") (str/join) (biginteger))])

(defn options [[time record]]
  (let [options (atom 0)]
    (dotimes [t time]
      (when (> (* t (- time t)) record)
        (swap! options inc)))
    @options))

(comment
  (time
   (with-open [rdr (io/reader "resources/day_6.txt")]
     (->> (line-seq rdr)
          (parse-lines)
          (options))))

  ;; => 33149631
  ;; "Elapsed time: 3121.069624 msecs"
  )
