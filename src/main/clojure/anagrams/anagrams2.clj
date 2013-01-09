(ns anagrams.anagrams2
  (:use clojure.test))

(defn char-occurrences-of [word]
  (reduce
    (fn [map letter] (assoc map letter (+ 1 (get map letter 0))))
    (hash-map)
    (seq word)
))

(defn group-by-char-occurrences [words]
  (reduce
    (fn [map word]
      (let [char-occurrences (char-occurrences-of word)]
        (assoc map char-occurrences (cons word (get map char-occurrences (list))))))
    (hash-map)
    words
))
(defn find-acronyms [word words-by-char-occurrences]
  (let [char-occurrences (char-occurrences-of word)
        words (get words-by-char-occurrences char-occurrences (list))]
    (remove #(= % word) words)
))

(defn read-dictionary-from [file-name]
  (map #(.trim %) (set (.split (.toLowerCase (slurp file-name)) "\n"))))

(def dictionary-file "/usr/share/dict/words")
(defn find-acronyms-of [word]
  (let [words (read-dictionary-from dictionary-file)
        filtered-words (filter #(= (.length word) (.length %)) words)]
  (find-acronyms word (group-by-char-occurrences filtered-words))
))

;(println (count (read-dictionary-from dictionary-file)))
;(println (find-acronyms-of "actaeonidae"))
;(println (find-acronyms-of "donatiaceae"))


(deftest should-find-acronyms-in-real-dictionary
  (is (= ["donatiaceae"] (find-acronyms-of "actaeonidae")))
  (is (= ["actaeonidae"] (find-acronyms-of "donatiaceae")))
)

(deftest should-find-acronyms-of-word
  (is (= [] (find-acronyms "ab" {})))
  (is (= ["ba"] (find-acronyms "ab" {{\b 1 \a 1} ["ab" "ba"], {\a 3} ["aaa"]})))
  (is (= ["ab"] (find-acronyms "ba" {{\b 1 \a 1} ["ab" "ba"], {\a 3} ["aaa"]})))
)
(deftest should-group-words-by-their-char-occurrences
  (is (= {} (group-by-char-occurrences [])))
  (is (= {{\a 3} ["aaa"]} (group-by-char-occurrences ["aaa"])))
  (is (= {{\b 1 \a 1} ["ab" "ba"], {\a 3} ["aaa"]} (group-by-char-occurrences ["aaa", "ba", "ab"])))
)
(deftest should-convert-word-into-numner-of-char-occurrences
  (is (= {} (char-occurrences-of "")))
  (is (= {\a 1} (char-occurrences-of "a")))
  (is (= {\a 3} (char-occurrences-of "aaa")))
  (is (= {\b 2 \a 1} (char-occurrences-of "abb")))
)
(run-tests)
