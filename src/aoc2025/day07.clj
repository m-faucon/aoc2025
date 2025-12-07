(ns aoc2025.day07
  (:require
   [clojure.string :as str]
   [aoc2025.util :as util]))

(defn parse-input
  [s]
  (let [[f & r]
        (->> (str/split-lines s)
             (partition 2)
             (map first))]
    {:start
     (str/index-of f \S)
     :splitter-rows
     (map (fn [s]
            (->> s
                 (keep-indexed
                  (fn [i c] (when (= \^ c) i)))
                 set))
          r)}))

(defn rf
  [{:keys [beams total]} splitters]
  (let [{splitted true unsplitted false}
        (group-by (comp boolean splitters) beams)]
    {:total (+ total (count splitted))
     :beams (->> splitted
                 (mapcat (fn [x] [(dec x) (inc x)]))
                 (concat unsplitted)
                 (into #{}))}))

(let [{:keys [start splitter-rows]}
      (parse-input (util/slurp-input))]
  (-> (reduce rf
              {:total 0 :beams [start]}
              splitter-rows)
      :total))

;; part2

;; now beams is a map of x to number of timelines
;; rather than simply a sequence of xs

(defn rf2
  [beams splitters]
  (let [{splitted true unsplitted false}
        (group-by (comp boolean splitters key) beams)]
    (->> splitted
         (mapcat (fn [[x n]] [[(dec x) n] [(inc x) n]]))
         (concat unsplitted)
         (reduce (fn [acc [x n]] (update acc x (fnil + 0) n)) {}))))

(let [{:keys [start splitter-rows]}
      (parse-input (util/slurp-input))]
  (->> (reduce rf2
               {start 1}
               splitter-rows)
       vals
       (reduce +)))
