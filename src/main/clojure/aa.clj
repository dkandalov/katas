;(println "hello clojure" (+ 1 3))

(defn sum-down-from [init-x]
  (loop [sum 0, x init-x]
    (if (pos? x)
      (recur (+ sum x) (dec x))
      sum
)))

(println (sum-down-from 3))