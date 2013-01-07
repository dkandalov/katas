(ns anagrams.anagrams0
  (:use clojure.test))

(defn read-words [] (set (filter #(> (.length %) 1) (map #(.toLowerCase %) (.split (slurp "/usr/share/dict/words") "\n")))))
(defn word-to-char-map [word]
  (reduce
    (fn [acc, letter] (assoc acc letter (+ 1 (get acc letter 0))))
    (hash-map)
    (seq word)))

(defn build-word-frequencies-map [words] ;TODO this is really slow (about 2 min)
  (reduce
    (fn [acc, word]
      (let [word-char-map (word-to-char-map word)]
        (assoc acc word-char-map (cons word (get acc word-char-map (list))))))
    (hash-map)
    words))
(defn anagrams-of [word, frequencies-map]
  (let [word-char-map (word-to-char-map word)
        anagrams (get frequencies-map word-char-map)]
  (remove #(= % word) anagrams)
))

;(println (count (read-words)))
;(println (count (build-word-frequencies-map (read-words))))
(let [frequencies-map (build-word-frequencies-map (read-words))]
  (println (anagrams-of "kinship" frequencies-map))
  (println (anagrams-of "enlist" frequencies-map))
  (println (anagrams-of "sort" frequencies-map))
)

(deftest should-find-anagrams-of-a-word
  (let [frequencies-map (build-word-frequencies-map (set ["cat" "tac"]))]
    (is (= (list "tac") (anagrams-of "cat" frequencies-map)))
))
(deftest should-build-word-frequencies-map
  (is (= {} (build-word-frequencies-map (set []))))
  (is (= {{\a 1} ["a"]} (build-word-frequencies-map (set ["a"]))))
  (is (= {{\a 3} ["aaa"]} (build-word-frequencies-map (set ["aaa"]))))
  (is (= {{\a 2, \b 1} ["baa" "aba"], {\b 3} ["bbb"]} (build-word-frequencies-map (set ["aba", "baa", "bbb"]))))
)
(deftest should-convert-word-into-map-of-char-frequencies
  (is (= {} (word-to-char-map "")))
  (is (= {\a 1} (word-to-char-map "a")))
  (is (= {\a 3} (word-to-char-map "aaa")))
  (is (= {\a 2 \b 1} (word-to-char-map "aba")))
)
(run-tests)