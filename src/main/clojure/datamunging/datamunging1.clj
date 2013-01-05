(ns datamunging.datamunging1
  (:use clojure.test))

(defrecord Entry [id value1 value2])

(defn read-lines-from [file-name] (.split (slurp file-name) "\n"))
(defn split-each-line [lines] (vec (map #(vec (.split (.trim %) "\\s+")) lines)))
(defn remove-non-digits [s] (.replaceAll s "\\*" ""))
(defn parse-row [row]
  (let [key (first row)
        value1 (remove-non-digits (nth row 1))
        value2 (remove-non-digits (nth row 2))]
    (Entry. key (Integer/parseInt value1) (Integer/parseInt value2))
))
(defn find-row-with-min-diff [columns-with-data]
  (let [parsed-lines (map parse-row columns-with-data)]
    (apply min-key #(Math/abs (- (:value1 %) (:value2 %))) parsed-lines)
))
(defn find-entity-with-min-diff [file-name filter-data extract-columns]
  (let [columns-with-data (extract-columns (split-each-line (filter-data (read-lines-from file-name))))
        row-with-min-values-diff (find-row-with-min-diff columns-with-data)]
    (:id row-with-min-values-diff)
))

(deftest should-find-team-with-min-goall-diff
  (def football-file "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/football.dat")
  (defn remove-non-data [lines] (filter #(not (.contains % "----")) (drop 5 (drop-last 1 lines))))
  (defn take-columns-with-data [lines] (map #(vector (nth % 1) (nth % 6) (nth % 8)) lines))

  (is (= "Aston_Villa" (find-entity-with-min-diff football-file remove-non-data take-columns-with-data)))
)

(deftest should-find-day-with-min-temperature-spread
  (def weather-file "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat")
  (defn remove-non-data [lines] (drop 8 (drop-last 2 lines)))
  (defn take-columns-with-data [lines] (map #(vector (nth % 0) (nth % 1) (nth % 2)) lines))

  (is (= "14" (find-entity-with-min-diff weather-file remove-non-data take-columns-with-data)))
)
(run-tests)