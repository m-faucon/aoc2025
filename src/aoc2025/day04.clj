(ns aoc2025.day04
  (:require
   [clojure.string :as str]
   [aoc2025.util :as util]))

;; NOTE: I'm using a hashmap with [x y] as keys
;; because it's more convenient.
;; Use a 2d array if performance matters

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (map-indexed
        (fn [y line]
          (map-indexed (fn [x c] [[x y] c]) line)))
       (into {} cat)))

(defn eight-neighbours
  [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y]       #_center    [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn accessible?
  [carte tile]
  (< (->> (eight-neighbours tile)
          (filter (comp #{\@} carte))
          count)
     4))

(let [carte (parse-input (util/slurp-input))]
  (->> carte
       (keep (fn [[tile c]] (when-not (= \. c) tile)))
       (filter (partial accessible? carte))
       count))

;; part2

(defn step
  [{:keys [carte] :as state}]
  (let [new-dots
        (->> carte
             (keep (fn [[tile c]] (when-not (= \. c) tile)))
             (filter (partial accessible? carte))
             (map (fn [tile] [tile \.])))]
    (-> state
        (assoc :removed-count (count new-dots))
        (update :carte into new-dots))))

(->> {:removed-count 0 :carte (parse-input (util/slurp-input))}
     (iterate step)
     rest
     (take-while (comp not zero? :removed-count))
     (map :removed-count)
     (reduce +))
