(ns merge_sort.merge-sort0
  (:use clojure.test))

(defn merge-sort [values]
  (defn do-merge [part1 part2]
    part1
  )
  (defn firstPartOf [values] values)
  (defn secondPartOf [values] values)

  (if (< (count values) 2) values
  (do-merge (firstPartOf values) (secondPartOf values))
))

(deftest given-a-vector-should-sort-it
  (is (= (merge-sort []) []))
  (is (= (merge-sort [1]) [1]))

  (is (= (merge-sort [1 2]) [1 2]))
  (is (= (merge-sort [2 1]) [1 2]))

  (is (= (merge-sort [1 2 3]) [1 2 3]))
  (is (= (merge-sort [1 3 2]) [1 2 3]))
  (is (= (merge-sort [2 1 3]) [1 2 3]))
  (is (= (merge-sort [2 3 1]) [1 2 3]))
  (is (= (merge-sort [3 1 2]) [1 2 3]))
  (is (= (merge-sort [3 2 1]) [1 2 3]))
)
(run-tests)