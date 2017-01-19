(ns bsearch.insert-sort1
  (:use clojure.test))

(defn insert-sort [values]
  (defn insert-into [sorted-values value]
    (cond
      (empty? sorted-values) (list value)
      (<= value (first sorted-values)) (cons value sorted-values)
      :else (cons (first sorted-values) (insert-into (rest sorted-values) value))
    )
  )

  (if (< (count values) 2)
      values
      (insert-into (insert-sort (rest values)) (first values))
))

(deftest given-a-list-should-sort-it
  (is (= (list) (insert-sort (list))))

  (is (= (list 1) (insert-sort (list 1))))

  (is (= (list 1 2) (insert-sort (list 1 2))))
  (is (= (list 1 2) (insert-sort (list 2 1))))

  (is (= (list 1 2 3) (insert-sort (list 1 2 3))))
  (is (= (list 1 2 3) (insert-sort (list 1 3 2))))
  (is (= (list 1 2 3) (insert-sort (list 2 1 3))))
  (is (= (list 1 2 3) (insert-sort (list 2 3 1))))
  (is (= (list 1 2 3) (insert-sort (list 3 1 2))))
  (is (= (list 1 2 3) (insert-sort (list 3 2 1))))
)

(deftest given-a-vector-should-sort-it
  (is (= [] (insert-sort [])))

  (is (= [1] (insert-sort (list 1))))

  (is (= [1 2] (insert-sort [1 2])))
  (is (= [1 2] (insert-sort [2 1])))

  (is (= [1 2 3] (insert-sort [1 2 3])))
  (is (= [1 2 3] (insert-sort [1 3 2])))
  (is (= [1 2 3] (insert-sort [2 1 3])))
  (is (= [1 2 3] (insert-sort [2 3 1])))
  (is (= [1 2 3] (insert-sort [3 1 2])))
  (is (= [1 2 3] (insert-sort [3 2 1])))
)
(run-tests)
