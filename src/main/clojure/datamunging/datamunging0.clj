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


(deftest should-find-day-with-min-temperature-spread
  (is (= "14" (find-day-with-min-temperature-spread)))
)
(run-tests)
