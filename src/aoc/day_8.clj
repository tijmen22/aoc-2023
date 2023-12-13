(ns aoc.day-8 
  (:require
   [clojure.java.io :as io]))

(def EXAMPLE
  ["LLR"
   ""
   "AAA = (BBB, BBB)"
   "BBB = (AAA, ZZZ)"
   "ZZZ = (ZZZ, ZZZ)"])

(defn- parse-network [lines]
  (->> lines
       (map #(re-find #"(\w+) = \((\w+), (\w+)\)" %))
       (map rest)
       (map #(zipmap [:from :left :right] %))
       (reduce (fn [idx node] (assoc idx (:from node) (select-keys node [:left :right]))) {})))

(defn- parse-instructions [instructions]
  (map #(get {\L :left \R :right} %) instructions))

(defn- parse-lines [[instructions _ & network]]
  {:instructions (parse-instructions instructions)
   :network (parse-network network)})

(defn- next-node [network node instruction]
  (get-in network [node instruction]))

(defn- instruction-reducer [network]
  (fn [nodes instruction]
    (conj nodes (next-node network (last nodes) instruction))))

(defn- reduce-instructions [starting-nodes network instructions]
  (let [traveled-nodes (reduce (instruction-reducer network) starting-nodes instructions)]
    (if (= "ZZZ" (last traveled-nodes))
      traveled-nodes
      (reduce-instructions traveled-nodes network instructions))))

(comment
  (with-open [rdr (io/reader "resources/day_8.txt")]
    (let [{:keys [instructions network]} (->> (line-seq rdr) (into []) (parse-lines))]
      (->>
       (reduce-instructions ["AAA"] network instructions)
       (count)
       (dec))))

;;
  )
