(ns insert_sort
  (:use clojure.test))

(defn sorted [values]
  (values))

(deftest should-sort-a-list
  (is (= (sorted (list)) (list)))
)

(run-tests)
