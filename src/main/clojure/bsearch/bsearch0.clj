(ns bsearch.bsearch0
  (:use clojure.test))

(defn bsearch
  ([element vector]
    (bsearch element vector 0 (count vector)))

  ([element vector from to]
    (if (or (empty? vector) (>= from to))
      -1
      (let [mid-index (quot (+ from to) 2)
            mid-element (nth vector mid-index)]
        (cond
          (= element mid-element) mid-index
          (< element mid-element) (bsearch element vector from mid-index)
          (> element mid-element) (bsearch element vector (+ 1 mid-index) to)
  ))))
)

(deftest should-find-index-of-element-in-a-vector
  (is (= (bsearch 0 []) -1))

  (is (= (bsearch 0 [1]) -1))
  (is (= (bsearch 1 [1]) 0))
  (is (= (bsearch 2 [1]) -1))

  (is (= (bsearch 0 [1 2]) -1))
  (is (= (bsearch 1 [1 2]) 0))
  (is (= (bsearch 2 [1 2]) 1))
  (is (= (bsearch 3 [1 2]) -1))

  (is (= (bsearch 0 [1 2 3]) -1))
  (is (= (bsearch 1 [1 2 3]) 0))
  (is (= (bsearch 2 [1 2 3]) 1))
  (is (= (bsearch 3 [1 2 3]) 2))
  (is (= (bsearch 4 [1 2 3]) -1))

  (is (= (bsearch 0 [1 2 3 4]) -1))
  (is (= (bsearch 1 [1 2 3 4]) 0))
  (is (= (bsearch 2 [1 2 3 4]) 1))
  (is (= (bsearch 3 [1 2 3 4]) 2))
  (is (= (bsearch 4 [1 2 3 4]) 3))
  (is (= (bsearch 5 [1 2 3 4]) -1))
)

(run-tests)
