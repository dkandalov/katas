(ns quick_sort.quicksort0
  (:use clojure.test))

(defn qsort [values]
  (cond
    (< (count values) 2) values
    :else (let [pivot-index (quot (count values) 2)
                pivot-value (nth values pivot-index)
                new-values (concat (subvec values 0 pivot-index) (subvec values (+ 1 pivot-index)))
                left-part (vec (filter #(<= % pivot-value) new-values))
                right-part (vec (filter #(> % pivot-value) new-values))]
      (concat (qsort left-part) [pivot-value] (qsort right-part))
    )
))

(deftest given-a-vector-should-sort-it
  (is (= [] (qsort [])))
  (is (= [1] (qsort [1])))

  (is (= [1, 2] (qsort [1, 2])))
  (is (= [1, 2] (qsort [2, 1])))

  (is (= [1, 2, 3] (qsort [1, 2, 3])))
  (is (= [1, 2, 3] (qsort [1, 3, 2])))
  (is (= [1, 2, 3] (qsort [2, 1, 3])))
  (is (= [1, 2, 3] (qsort [2, 3, 1])))
  (is (= [1, 2, 3] (qsort [3, 1, 2])))
  (is (= [1, 2, 3] (qsort [3, 2, 1])))

  (is (= [2, 3, 4, 11] (qsort [3, 2, 4, 11])))
  (is (= [-1, 2, 3, 4, 11] (qsort [3, 2, 4, -1, 11])))
)
(run-tests)