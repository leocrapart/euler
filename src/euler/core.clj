(ns euler.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; e42


(def alphabet-string
  "abcdefghijklmnopqrstuvwxyz")
(seq alphabet-string)
(map identity alphabet-string)

(defn character-value [char]
  (inc
    (.indexOf (seq alphabet-string) char)))  

(character-value (first "k"))


(defn word-value [word]
  (reduce +
    (map character-value word)))

(word-value "aaa")

(defn triangle-number [n]
  (/ (* n (inc n)) 2))

(triangle-number 10)

(def triangle-numbers-100
  (vec
    (map triangle-number (range 1 101))))

triangle-numbers-100

(defn is-triangle-word? [word]
  (if (some #{(word-value word)} triangle-numbers-100)
    true
    false))

(is-triangle-word? "skyyy")
(word-value "skyyy")
triangle-numbers-100



(def lines
  (clojure.string/split-lines
      (slurp "p042_words.txt")))

(defn word-without-double-quotes [word]
  (clojure.string/join ""
    (drop 1 
      (take (dec (count word)) word))))

(word-without-double-quotes "\"ABLE\"")


(def words
  (map clojure.string/lower-case
    (map word-without-double-quotes
      (flatten
        (map (fn [line] (clojure.string/split line #","))
          lines)))))
lines
words


(def triangle-words-count
  (count
    (filter identity
      (map is-triangle-word? words))))

;; sol e42
triangle-words-count




