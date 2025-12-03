(ns aoc2025.day03
  (:require
   [clojure.string :as str]
   [aoc2025.util :as util]))

;; n = 100 so n^2 and unoptimized is fine
(defn joltage
  [line]
  (->> (for [i (range (count line))
             j (range (inc i) (count line))]
         (parse-long (str (.charAt line i) (.charAt line j))))
       (reduce max)))

(->> (util/slurp-input)
     str/split-lines
     (map joltage)
     (reduce +))

;; part2

;; now we think, because n^12 doesn't work

;; so, if there's a 9 somewhere before the (length - 12) index, surely the first
;; digit is a 9. Further, we can assume that the first 9 of the string can be used
;; from there we can recurse

(defn leading-digit
  [s remaining-digits]
  (->> (subs s 0 (+ (count s) (- remaining-digits) 1))
       (map long)
       (reduce max)
       char))

(defn joltage-part2
  ([s remaining-digits]
   (joltage-part2 s remaining-digits []))
  ([s remaining-digits acc]
   (if (zero? remaining-digits)
     (parse-long (apply str acc))
     (let [digit (leading-digit s remaining-digits)
           first-occurence (.indexOf s (int digit))]
       (joltage-part2 (subs s (inc first-occurence))
                      (dec remaining-digits)
                      (conj acc digit))))))

(->> (util/slurp-input)
     str/split-lines
     (map #(joltage-part2 % 12))
     (reduce +))
