(ns merge_sort.merge-sort1
  (:use clojure.test))

(defn merge-sort [values]
  (defn mid-index [values] (quot (count values) 2))
  (defn first-part [values] (subvec values 0 (mid-index values)))
  (defn second-part [values] (subvec values (mid-index values)))
  (defn do-merge [part1 part2]
    (cond
      (empty? part2) part1
      (empty? part1) part2
      (<= (first part1) (first part2)) (cons (first part1) (do-merge (rest part1) part2))
      :else (cons (first part2) (do-merge part1 (rest part2)))
  ))

  (if (< (count values) 2)
    values
    (do-merge (merge-sort (first-part values)) (merge-sort (second-part values)))
))

(deftest given-a-vector-should-sort-it
  (is (= [] (merge-sort [])))
  (is (= [1] (merge-sort [1])))

  (is (= [1 2] (merge-sort [1 2])))
  (is (= [1 2] (merge-sort [2 1])))

  (is (= [1 2 3] (merge-sort [1 2 3])))
  (is (= [1 2 3] (merge-sort [1 3 2])))
  (is (= [1 2 3] (merge-sort [2 1 3])))
  (is (= [1 2 3] (merge-sort [2 3 1])))
  (is (= [1 2 3] (merge-sort [3 1 2])))
  (is (= [1 2 3] (merge-sort [3 2 1])))

  (is (= [1 2 3 11] (merge-sort [11 2 1 3])))
  (is (= [1 2 3 4 12] (merge-sort [3 2 1 12 4])))
)
(run-tests)