(ns bsearch.bsearch1
  (:use clojure.test))

(defn binary-search
  ([element vector] (binary-search element vector 0))
  ([element vector shifted-by]
  (if (empty? vector) -1
    (let [mid-index (quot (count vector) 2)
          mid-element (nth vector mid-index)]
      (cond
        (= element mid-element) (+ mid-index shifted-by)
        (< element mid-element) (binary-search element (subvec vector 0 mid-index) shifted-by)
        (> element mid-element) (binary-search element (subvec vector (+ 1 mid-index)) (+ 1 mid-index shifted-by))
)))))

(deftest should-find-index-of-element-in-a-vector
  (is (= -1 (binary-search 1 [])))

  (is (= -1 (binary-search 0 [1])))
  (is (= 0  (binary-search 1 [1])))
  (is (= -1 (binary-search 2 [1])))

  (is (= -1 (binary-search 0 [1 2])))
  (is (= 0  (binary-search 1 [1 2])))
  (is (= 1  (binary-search 2 [1 2])))
  (is (= -1 (binary-search 3 [1 2])))

  (is (= -1 (binary-search 0 [1 2 3])))
  (is (= 0  (binary-search 1 [1 2 3])))
  (is (= 1  (binary-search 2 [1 2 3])))
  (is (= 2  (binary-search 3 [1 2 3])))
  (is (= -1 (binary-search 4 [1 2 3])))

  (is (= -1 (binary-search 0 [1 2 3 4])))
  (is (= 0  (binary-search 1 [1 2 3 4])))
  (is (= 1  (binary-search 2 [1 2 3 4])))
  (is (= 2  (binary-search 3 [1 2 3 4])))
  (is (= 3  (binary-search 4 [1 2 3 4])))
  (is (= -1 (binary-search 5 [1 2 3 4])))
)
(run-tests)