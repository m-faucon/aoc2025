(ns aoc2025.day08
  (:require
   [aoc2025.util :as util]
   [clojure.string :as str]))

(defn distance²
  [P Q]
  (reduce + (map (comp #(* % %) -) P Q)))

;; data structure for the connected components is 2 maps
;; `:M->id`: map of point to cc id
;; `:id->Ms`: map of cc id to vector of points
;; I think we need this sort of doubly linked structure to cut
;; an `n` from the asymptotic complexity
 
(defn init-ccs
  [xs]
  {:M->id (zipmap xs xs)
   :id->Ms (zipmap xs (map vector xs))})

(defn connect
  [{:keys [M->id id->Ms] :as state} [P Q]]
  ;; possible optim I didn't bother with, is find out
  ;; which one is larger to minimize edits
  (let [P-id (M->id P)
        Q-id (M->id Q)
        Qs (id->Ms Q-id)]
    (cond-> state
      (not= P-id Q-id)
      (->
       (update :id->Ms dissoc Q-id)
       (update-in [:id->Ms P-id] into Qs)
       (update :M->id into (map (fn [M] [M P-id]) Qs))))))

(defn pairs
  [xs]
  (for [x xs y xs :when (< (compare x y) 0)] [x y]))

(let [points
      (->> (util/slurp-input)
           (str/split-lines)
           (map (fn [s] (->> (str/split s #",") (mapv parse-long)))))
      pairs-sorted
      (->> (pairs points)
           (sort-by (fn [[P Q]] (distance² P Q)))
           (take 1000))
      ccs (init-ccs points)]
  (->> (reduce connect ccs pairs-sorted)
       :id->Ms
       (map (comp count val))
       (sort-by -) (take 3) (reduce *)))

;; part2

(defn connect2
  [acc pair]
  (let [new (connect acc pair)]
    (cond-> new
      (not= acc new)
      (assoc :last-connect pair))))

(let [points
      (->> (util/slurp-input)
           (str/split-lines)
           (map (fn [s] (->> (str/split s #",") (mapv parse-long)))))
      pairs-sorted
      (->> (pairs points)
           (sort-by (fn [[P Q]] (distance² P Q))))
      ccs (init-ccs points)]
  (->> (reduce connect2 ccs pairs-sorted)
       :last-connect
       (map first)
       (apply *)))
