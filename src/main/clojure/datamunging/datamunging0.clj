(ns datamunging.datamunging0
  (:use [clojure.string :only [split]]
        clojure.test))

(defn read-file-lines [file-name] (vec (.split (slurp file-name) "\n")))
(defn as-int [s] (. Integer parseInt (.replace s "*" "")))
(defn split-each-line [lines] (map #(vec (.split (.trim %) "\\s+")) lines))
(defn find-row-with-min-diff [data-rows]
  (apply min-key #(Math/abs (- (nth % 1) (nth % 2))) data-rows))


(defn find-day-with-min-temperature-spread []
  (defn filter-non-data-lines [lines] (drop 8 (drop-last 2 lines)))
  (defn parse-as-data [lines] (map #(vec [(nth % 0) (as-int (nth % 1)) (as-int (nth % 2))]) (split-each-line lines)))

  (let [lines (read-file-lines "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat")]
    (first (find-row-with-min-diff (parse-as-data (filter-non-data-lines lines))))
  )
)

(defn find-team-with-min-goal-diff []
  (defn filter-non-data-lines [lines] (drop-last 1 (drop 5 (filter #(not (.contains % "---")) lines))))
  (defn parse-as-data [lines]
    (map #(vec [(nth % 1) (as-int (nth % 6)) (as-int (nth % 8))])(split-each-line lines)))

  (let [lines (read-file-lines "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat")]
    (first (find-row-with-min-diff (parse-as-data (filter-non-data-lines lines))))
  )
)

(deftest should-find-team-with-min-goal-diff
  (is (= "Aston_Villa" (find-team-with-min-goal-diff)))
)
(deftest should-find-day-with-min-temperature-spread
  (is (= "14" (find-day-with-min-temperature-spread)))
)
(run-tests)
