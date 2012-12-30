(ns merge_sort.merge-sort0
  (:use clojure.test))

(defn merge-sort [values]
  (defn do-merge [part1 part2]
    (cond
      (empty? part1) part2
      (empty? part2) part1
      (<= (first part1) (first part2)) (cons (first part1) (do-merge (rest part1) part2))
      :else (cons (first part2) (do-merge part1 (rest part2)))
  ))
  (defn mid-index-of [values] (quot (count values) 2))
  (defn first-part-of [values] (subvec values 0 (mid-index-of values)))
  (defn second-part-of [values] (subvec values (mid-index-of values)))

  (if (< (count values) 2) values
  (do-merge (merge-sort (first-part-of values)) (merge-sort (second-part-of values)))
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