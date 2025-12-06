(ns aoc2025.day05
  (:require
   [clojure.string :as str]
   [aoc2025.util :as util]))

(defn parse-input
  [s]
  (let [[fresh-ranges availables] (str/split s #"\n\n")]
    {:fresh-ranges
     (->> (str/split-lines fresh-ranges)
          (map (fn [s] (mapv parse-long (str/split s #"-")))))
     :availables
     (->> (str/split-lines availables)
          (map parse-long))}))

(defn fresh?
  [fresh-ranges id]
  (some (fn [[m M]] (<= m id M))
        fresh-ranges))

(let [{:keys [fresh-ranges availables]}
      (parse-input (util/slurp-input))]
  (->> availables
       (filter (partial fresh? fresh-ranges))
       count))

;; part2

;; The plan : build a set of disjoint intervals with same
;; union as the union of input intervals.
;; input intervals are consumed one by one and the invariant
;; holds after each step

;; Re reading the next day: this could have been made much simpler
;; by at each step only recording the intervals intersecting the new one
;; and the min / max, rather than the details of how they intersect
;; oh well

;; also, sorting first would help performance but it's instant anyways
 
(defn intersection-type
  [[Im IM] [Jm JM]]
  (cond
    (or (< IM Jm) (< JM Im))
    :empty
    (<= Jm Im IM JM)
    :i-inside-j
    (<= Im Jm JM IM)
    :j-inside-i
    (and (< Im Jm) (<= Jm IM) (< IM JM))
    :left
    (and (< Jm Im) (<= Im JM) (< JM IM))
    :right
    :else (throw (ex-info "Unreachable" {}))))

(defn intersection-data
  "Given Is (sequence of disjoint intervals) and J, returns :
{:left nilable-interval
 :right nilable-interval
 :including nilable-interval
 :included sequence-of-intervals}.
  Note that if :including is non nil, then all other fields
  are nil / empty."
  [Is J]
  (reduce (fn [acc I]
            (case (intersection-type I J)
              :empty acc
              :left (assoc acc :left I)
              :right (assoc acc :right I)
              :i-inside-j (update acc :included (fnil conj []) I)
              :j-inside-i {:including I}))
          {}
          Is))

(->> (parse-input (util/slurp-input))
     :fresh-ranges
     (reduce
      (fn [Is J]
        (let [{:keys [left right included including]}
              (intersection-data Is J)
              union-m (first (or including left J))
              union-M (second (or including right J))]
          (-> (apply disj Is
                     left right including
                     included)
              (conj [union-m union-M]))))
      #{})
     (map (fn [[x y]] (inc (- y x))))
     (reduce +))
