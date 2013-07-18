(ns bsearch.bsearch2
  (:use clojure.test))


(defn search
  ([value vector]
    (search value vector 0))
  ([value vector shift]
  (if (empty? vector) -1
    (let [mid-index (unchecked-divide-int (count vector) 2)
          mid-value (nth vector mid-index)]
      (cond (= value mid-value) (+ mid-index shift)
            (< value mid-value) (search value (take mid-index vector) shift)
                          :else (search value (drop (+ mid-index 1) vector) (+ shift mid-index 1))
)))))

(deftest should-find-index-of-element-in-a-vector
  (is (= (search 1 []) -1))

  (is (= (search 0 [1]) -1))
  (is (= (search 1 [1]) 0))
  (is (= (search 2 [1]) -1))

  (is (= (search 0 [1 2]) -1))
  (is (= (search 1 [1 2]) 0))
  (is (= (search 2 [1 2]) 1))
  (is (= (search 3 [1 2]) -1))

  (is (= (search 0 [1 2 3]) -1))
  (is (= (search 1 [1 2 3]) 0))
  (is (= (search 2 [1 2 3]) 1))
  (is (= (search 3 [1 2 3]) 2))
  (is (= (search 4 [1 2 3]) -1))

  (is (= (search 0 [1 2 3 4]) -1))
  (is (= (search 1 [1 2 3 4]) 0))
  (is (= (search 2 [1 2 3 4]) 1))
  (is (= (search 3 [1 2 3 4]) 2))
  (is (= (search 4 [1 2 3 4]) 3))
  (is (= (search 5 [1 2 3 4]) -1))
)
(run-tests)

