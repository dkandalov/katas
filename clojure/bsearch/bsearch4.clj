(ns bsearch.bsearch4
  (:use clojure.test))

(defn bsearch
  ([value list] (bsearch value list 0))
  ([value list shift]
    (if (empty? list) -1
      (let [mid-index (quot (count list) 2)
            mid-value (nth list mid-index)]
      (cond
        (= value mid-value) (+ mid-index shift)
        (< value mid-value) (bsearch value (take mid-index list) shift)
        (> value mid-value) (bsearch value (drop (+ 1 mid-index) list) (+ 1 mid-index shift)))
  )))
)

(deftest aa
  (is (= -1 (bsearch 1 [])))

  (is (= -1 (bsearch 0 [1])))
  (is (= 0  (bsearch 1 [1])))
  (is (= -1 (bsearch 2 [1])))

  (is (= -1 (bsearch 0 [1, 2])))
  (is (= 0  (bsearch 1 [1, 2])))
  (is (= 1  (bsearch 2 [1, 2])))
  (is (= -1 (bsearch 3 [1, 2])))

  (is (= -1 (bsearch 0 [1, 2, 3])))
  (is (= 0  (bsearch 1 [1, 2, 3])))
  (is (= 1  (bsearch 2 [1, 2, 3])))
  (is (= 2  (bsearch 3 [1, 2, 3])))
  (is (= -1 (bsearch 4 [1, 2, 3])))

  (is (= -1 (bsearch 0 [1, 2, 3, 4])))
  (is (= 0  (bsearch 1 [1, 2, 3, 4])))
  (is (= 1  (bsearch 2 [1, 2, 3, 4])))
  (is (= 2  (bsearch 3 [1, 2, 3, 4])))
  (is (= 3  (bsearch 4 [1, 2, 3, 4])))
  (is (= -1 (bsearch 5 [1, 2, 3, 4])))
)

(run-tests)