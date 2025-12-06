(ns aoc2025.day06
  (:require
   [aoc2025.util :as util]
   [clojure.string :as str]))

(->> (util/slurp-input)
     (str/split-lines)
     (map (fn [s] (str/split s #" +")))
     (apply mapv vector)
     (map (fn [col]
            (let [op (case (peek col) "*" * "+" +)
                  nums (map parse-long (pop col))]
              (apply op nums))))
     (reduce +))

;; part2
 
(->> (util/slurp-input)
     (str/split-lines)
     (apply mapv vector)
     (partition-by (partial every? #{\space}))
     (remove (comp #{1} count))
     (map (fn [cols]
            (let [op (case (-> cols first peek)
                       \* * \+ +)]
              (->> cols
                   (map (fn [col]
                          (->> (pop col)
                               (remove #{\space})
                               (apply str)
                               parse-long)))
                   (reduce op)))))
     (reduce +))
