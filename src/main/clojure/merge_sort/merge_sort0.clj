(ns merge_sort.merge-sort0
  (:use clojure.test))

(defn merge-sort [values]
  values
)

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