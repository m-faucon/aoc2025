(ns aoc2025.day01
  (:require [aoc2025.util :as util]
            [clojure.string :as str]))

(defn parse-rotation
  [s]
  (let [[_ direction amount] (re-matches #"([RL])(\d+)" s)]
    (* (case direction "R" 1 "L" -1)
       (parse-long amount))))

(->> (util/slurp-input)
     (str/split-lines)
     (map parse-rotation)
     (reductions (fn [acc x] (mod (+ acc x) 100)) 50)
     (filter #{0})
     count)

;; part2

(defn crossings
  [old unwrapped]
  (cond-> (abs (long (/ unwrapped 100)))
    (and (<= unwrapped 0) (not (zero? old)))
    inc))
(long (/ -105 100))

(->> (util/slurp-input)
     (str/split-lines)
     (map parse-rotation)
     (reduce (fn [{:keys [dial total-crossings]} change]
               (let [unwrapped (+ dial change)]
                 {:dial (mod unwrapped 100)
                  :total-crossings (+ total-crossings
                                      (crossings dial unwrapped))}))
             {:dial 50 :total-crossings 0})
     :total-crossings)
