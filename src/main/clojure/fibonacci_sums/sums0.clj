(ns fibonacci_sums.sums0
  (:use clojure.test)
  (:require clojure.string))

(defn calculate-fibonacci [count]
  (list 1 2 3 5 8 13 21 34 55 89)
)

(defn combinations [fibs number]
  (let [filtered-fibs (take-while #(<= % number) fibs)
        last-number (last filtered-fibs)]
    (cond
      (= number 0) (list(list))
      (empty? filtered-fibs) (list)
      :else (concat
              (map #(cons last-number %) (combinations (drop-last filtered-fibs) (- number last-number)))
              (combinations (drop-last filtered-fibs) number)
      )
)))

(defn combination-as-string [fib-combination]
  (let [fibs-reverse (reverse (calculate-fibonacci 10))
        combination-as-string (map #(if (.contains fib-combination %) "1" "0") fibs-reverse)]
      (clojure.string/join (drop-while #(= "0" %) combination-as-string))
))

(defn fibonacci-representations-of [number]
  (let [fibs (calculate-fibonacci 10)]
    (map #(combination-as-string %) (combinations fibs number))
))

(deftest given-a-number-should-find-its-fibonacci-representations
  (let [fibs (calculate-fibonacci 10)]
    (is (= (fibonacci-representations-of 0) (list "")))
    (is (= (fibonacci-representations-of 1) (list "1")))
    (is (= (fibonacci-representations-of 2) (list "10")))
    (is (= (fibonacci-representations-of 3) (list "100" "11")))
    (is (= (fibonacci-representations-of 4) (list "101")))
    (is (= (fibonacci-representations-of 5) (list "1000" "110")))
))

(deftest given-a-combination-should-conver-it-to-string
  (is (= (combination-as-string (list)) ""))
  (is (= (combination-as-string (list 1)) "1"))
  (is (= (combination-as-string (list 2)) "10"))
  (is (= (combination-as-string (list 3)) "100"))
  (is (= (combination-as-string (list 3 1)) "101"))
)

(deftest given-a-number-should-find-combinations-of-fibonnaci-numbers-with-sum-equal-to-it
  (let [fibs (calculate-fibonacci 10)]
    (is (= (combinations fibs 0) (list(list))))
    (is (= (combinations fibs 1) (list(list 1))))
    (is (= (combinations fibs 2) (list(list 2))))
    (is (= (combinations fibs 3) (list(list 3)(list 2 1))))
    (is (= (combinations fibs 4) (list(list 3 1))))
    (is (= (combinations fibs 5) (list(list 5) (list 3 2))))
))

(deftest should-calculate-fibonacci-sequence
  (is (= (calculate-fibonacci 10) (list 1 2 3 5 8 13 21 34 55 89)))
)

(run-tests)
