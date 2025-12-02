(ns aoc2025.util
  (:require
   [clojure.string :as str]))

(defn day
  []
  (->> (str/split (str *ns*) #"\.")
       last
       (re-find #"\d+")))

(defn slurp-input
  []
  ; the trim is because when I copy paste into emacs and save it adds a newline at the end
  (str/trimr (slurp (str "resources/day" (day)))))
