(ns bsearch.insert-sort
  (:use clojure.test))

(defn insert-sort [a-list]
  (defn insert [value sorted-list]
    (cond (empty? sorted-list) (list value)
      (<= value (first sorted-list)) (cons value sorted-list)
      :else (cons (first sorted-list) (insert value (rest sorted-list)))
  ))

  (if (< (count a-list) 2)
    a-list
    (insert (first a-list) (insert-sort (rest a-list)))
))

(deftest should-sort-list
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
(run-tests)