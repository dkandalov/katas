(ns datamunging.datamunging0
  (:use [clojure.string :only [split]]
        clojure.test))

(defn find-day-with-min-temperature-spread []
  (defn file-as-string [] (slurp "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat"))
  (defn lines [] (drop 8 (drop-last 2 (seq (.split (file-as-string) "\n")))))
  (defn data [] (map #(vec (.split (.trim %) "\\s+")) (lines)))
  (defn temperatures []
    (defn as-int [s] (. Integer parseInt (.replace s "*" "")))
    (map #(vec [(nth % 0) (as-int (nth % 1)) (as-int (nth % 2))]) (data)))
  (defn row-with-min-temperature-diff []
    (apply min-key #(Math/abs (- (nth % 1) (nth % 2))) (temperatures)))

  (nth (row-with-min-temperature-diff) 0)
)

(defn find-team-with-min-goal-diff []
  (defn read-file [] (slurp "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat"))
  (defn lines [] (vec (.split (read-file) "\n")))
  (defn drop-header-and-footer [] (drop-last 1 (drop 5 (filter #(not (.contains % "---")) (lines)))))
  (defn data-as-strings [] (map #(vec (.split (.trim %) "\\s+")) (drop-header-and-footer)))
  (defn as-int [s] (Integer/parseInt s))
  (defn data [] (map #(vec [(nth % 1) (as-int (nth % 6)) (as-int (nth % 8))])(data-as-strings)))

  (first (apply min-key #(Math/abs (- (nth % 1) (nth % 2))) (data)))
)

(deftest should-find-team-with-min-goal-diff
  (is (= "Aston_Villa" (find-team-with-min-goal-diff)))
)
(deftest should-find-day-with-min-temperature-spread
  (is (= "14" (find-day-with-min-temperature-spread)))
)
(run-tests)
