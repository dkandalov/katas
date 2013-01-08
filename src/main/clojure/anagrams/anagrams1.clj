(ns anagrams.anagrams1
  (:use clojure.test))

(defn read-dictionary-from [file-name]
  (set (.split (.toLowerCase (slurp file-name)) "\n")))

(defn char-frequencies-of [word]
  (reduce
    (fn [map letter] (assoc map letter (+ 1(get map letter 0))))
    (hash-map)
    (seq word)
))
(defn frequencies-map-of [words]
  (reduce
    (fn [map word]
      (let [word-chars-map (char-frequencies-of word)]
        (assoc map word-chars-map (cons word (get map word-chars-map (list))))
    ))
    (hash-map)
    words
))
(defn find-anagrams-for [word frequencies-map]
  (remove #(= % word)
    (get frequencies-map (char-frequencies-of word) (list))
))
(defn do-find-anagrams-for [word]
  (let [word-length (.length word)
        filtered-words (filter #(= word-length (.length %)) (read-dictionary-from "/usr/share/dict/words"))]
    (find-anagrams-for word (frequencies-map-of filtered-words))
))

;(println (count (read-dictionary-from "/usr/share/dict/words")))
;(println (do-find-anagrams-for "fresher"))
;(println (do-find-anagrams-for "refresh"))

(deftest should-find-anagrams-using-real-dictionary
  (is (= ["refresh"] (do-find-anagrams-for "fresher")))
  (is (= ["fresher"] (do-find-anagrams-for "refresh"))) ; TODO this is broken!
)
(deftest should-find-anagrams-for-a-word
  (is (= [] (find-anagrams-for "aaa" {})))
  (is (= ["tac"] (find-anagrams-for "cat" (frequencies-map-of ["cat" "tac"]))))
)
(deftest should-map-of-words-by-their-frequencies
  (is (= {} (frequencies-map-of [])))
  (is (= {{\a 3} ["aaa"]} (frequencies-map-of ["aaa"])))
  (is (= {{\a 3} ["aaa"], {\b 1 \a 2} ["baa", "aba"]} (frequencies-map-of ["aba" "aaa" "baa"])))
)
(deftest should-convert-word-into-char-frequencies
  (is (= {} (char-frequencies-of "")))
  (is (= {\a 1} (char-frequencies-of "a")))
  (is (= {\a 3} (char-frequencies-of "aaa")))
  (is (= {\a 2, \b 1} (char-frequencies-of "aba")))
)
(run-tests)
