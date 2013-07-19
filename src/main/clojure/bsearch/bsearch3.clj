(ns bsearch.bsearch3
  (:use clojure.test))

(defn binary-search
  ([value seq]
    (binary-search value seq 0))
  ([value seq shift]
  (if (empty? seq) -1
    (let [mid-index (unchecked-divide-int (count seq) 2)
          mid-value (nth seq mid-index)]
      (cond (= value mid-value) (+ mid-index shift)
            (< value mid-value) (binary-search value (take mid-index seq) shift)
            :else (binary-search value (drop (+ 1 mid-index) seq) (+ 1 mid-index shift)))))))


(deftest should-find-index-of-element-in-a-vector
  (is (= (binary-search 1 []) -1))

  (is (= (binary-search 0 [1]) -1))
  (is (= (binary-search 1 [1]) 0))
  (is (= (binary-search 2 [1]) -1))

  (is (= (binary-search 0 [1 2]) -1))
  (is (= (binary-search 1 [1 2]) 0))
  (is (= (binary-search 2 [1 2]) 1))
  (is (= (binary-search 3 [1 2]) -1))

  (is (= (binary-search 0 [1 2 3]) -1))
  (is (= (binary-search 1 [1 2 3]) 0))
  (is (= (binary-search 2 [1 2 3]) 1))
  (is (= (binary-search 3 [1 2 3]) 2))
  (is (= (binary-search 4 [1 2 3]) -1))
)
(run-tests)
