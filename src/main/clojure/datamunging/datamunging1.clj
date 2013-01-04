(ns datamunging.datamunging1
  (:use clojure.test))

(deftest should-find-day-with-min-temperature-spread
  (def weather-file "/Users/dima/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat")

  (defn read-lines-from [file-name] (.split (slurp weather-file) "\n"))
  (defn remove-non-data [lines] (drop 8 (drop-last 2 lines)))
  (defn split-lines [lines] (vec (map #(vec (.split (.trim %) "\\s+")) lines)))
  (defn take-columns-with-data [lines] (map #(vector (nth % 0) (nth % 1) (nth % 2)) lines))

  (defn remove-non-digits [s] (.replaceAll s "\\*" ""))
  (defn parse-row [row]
    (let [key (first row)
          value1 (remove-non-digits (nth row 1))
          value2 (remove-non-digits (nth row 2))]
      (vector key (Integer/parseInt value1) (Integer/parseInt value2))
  ))

  (let [
        columns-with-data (take-columns-with-data (split-lines (remove-non-data (read-lines-from weather-file))))
        parsed-lines (map parse-row columns-with-data)
        row-with-min-values-diff (apply min-key #(Math/abs (- (nth % 1) (nth % 2))) parsed-lines)
        find-day-with-min-temperature-spread (first row-with-min-values-diff)
    ]

    (is (= (list "14", 61, 59) row-with-min-values-diff))

    (is (= (list "1", "88", "59") (nth columns-with-data 0)))
    (is (= (list "9", "86", "32*") (nth columns-with-data 8)))
    (is (= (list "30", "90", "45") (nth columns-with-data 29)))
    (is (= 30 (count columns-with-data)))

    (is (= (list "1", 88, 59) (nth parsed-lines 0)))
    (is (= (list "9", 86, 32) (nth parsed-lines 8)))
    (is (= (list "30", 90, 45) (nth parsed-lines 29)))
    (is (= 30 (count parsed-lines)))

    (is (= 30 (count (remove-non-data (read-lines-from weather-file)))))
    (is (= 40 (count (read-lines-from weather-file))))

    (is (= "14" find-day-with-min-temperature-spread))
    )
)
(run-tests)