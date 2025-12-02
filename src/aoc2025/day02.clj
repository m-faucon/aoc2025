(ns aoc2025.day02
  (:require
   [aoc2025.util :as util]
   [clojure.string :as str]))

(defn invalid-id?
  [id]
  (let [s (str id) l (count s)]
    (and (even? l)
         (let [halfpoint (/ l 2)]
           (= (subs s 0 halfpoint)
              (subs s halfpoint))))))

(->> (str/split (util/slurp-input) #",")
     (map (fn [s] (mapv parse-long (str/split s #"-"))))
     (mapcat (fn [[start end]] (range start (inc end))))
     (filter invalid-id?)
     (reduce +))

;; part2

;; lol regexes
(defn invalid-id-for-real?
  [id]
  (re-matches #"(\d+)(?:\1)+" (str id)))

(->> (str/split (util/slurp-input) #",")
     (map (fn [s] (mapv parse-long (str/split s #"-"))))
     (mapcat (fn [[start end]] (range start (inc end))))
     (filter invalid-id-for-real?)
     (reduce +))
