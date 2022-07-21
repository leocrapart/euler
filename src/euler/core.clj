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




; diagonals - vertical - horizontal
; str -> grid -> diagonals -> products -> max
;grid 
(def grid-str
"08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(defn parse-int [str]
  (Integer/parseInt str))

(defn split-into-numbers [line]
  (clojure.string/split line #" "))

(defn parse-line [line]
  (map parse-int
    (split-into-numbers line)))

(defn grid [grid-str]
  (let [lines (clojure.string/split grid-str #"\n")]
    (map parse-line lines)))

(grid grid-str)

(nth
  (nth (grid grid-str) 1)
  0)

(defn number-at [grid row-number index]
  (nth
    (nth grid row-number)
    index))

;diagonals
; 0,0 -> 1,1 -> 2,2 -> 3,3

(number-at (grid grid-str) 1 2)

(defn right-diagonal [grid row-number index]
  [(number-at grid row-number index) 
   (number-at grid (+ 1 row-number) (+ 1 index)) 
   (number-at grid (+ 2 row-number) (+ 2 index)) 
   (number-at grid (+ 3 row-number) (+ 3 index))])

(defn left-diagonal [grid row-number index]
  [(number-at grid row-number index) 
   (number-at grid (+ 1 row-number) (- index 1)) 
   (number-at grid (+ 2 row-number) (- index 2)) 
   (number-at grid (+ 3 row-number) (- index 3))])


(def grid (grid grid-str))

(right-diagonal grid 16 17)
(left-diagonal grid 0 3)

; row-numbers = [0, 16]
; indexes = [0, 16]
(defn right-diagonals-of-row [grid row-number]
  (let [indexes (range 17)]
    (map (partial right-diagonal grid row-number)
      indexes)))


(defn right-diagonals [grid]
  (let [row-numbers (range 17)]
    (sequence cat
      (map (partial right-diagonals-of-row grid)
        row-numbers))))

(defn left-diagonals-of-row [grid row-number]
  (let [indexes (range 3 20)]
    (map (partial left-diagonal grid row-number)
      indexes)))


(defn left-diagonals [grid]
  (let [row-numbers (range 17)]
    (sequence cat
      (map (partial left-diagonals-of-row grid)
        row-numbers))))

(right-diagonals-of-row grid 0)
(right-diagonals grid)

(left-diagonals-of-row grid 0)
(left-diagonals grid)

(defn diagonals [grid]
  (concat (left-diagonals grid) (right-diagonals grid)))

(diagonals grid)

;; products
(defn diagonal-product [diagonal]
  (reduce * diagonal))

(defn diagonals-products [diagonals]
  (map diagonal-product diagonals))

(defn max-product [diagonals]
  (let [diagonals-products (diagonals-products diagonals)]
    (apply max diagonals-products)))

(diagonal-product [1 2 3 4])
(diagonal-product [97 99 49 52])
(diagonals-products (diagonals grid))
(max-product (diagonals grid))
;;e11 solved



;;e45
(defn triangle [number]
  (/ (* number (+ number 1)) 2))

(defn triangles [n]
  (map triangle (range n)))

(defn pentagonal [number]
  (/ (* number (- (* 3 number) 1)) 2))

(defn pentagonals [n]
  (map pentagonal (range n)))

(defn hexagonal [number]
  (* number (- (* 2 number) 1)))

(defn hexagonals [n]
  (map hexagonal (range n)))

(triangles 1000)
(pentagonals 1000)
(hexagonals 1000)

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

(in? (hexagonals 1000) 15)


(defn pentagonal? [number max-index]
  (if (in? (pentagonals max-index) number)
    true
    false))


(defn hexagonal? [number max-index]
  (if (in? (hexagonals max-index) number)
    true
    false))


(defn hexagonal-and-pentagonal? [number max-index]
  (and (hexagonal? number max-index)
       (pentagonal? number max-index)))


(pentagonal? 40755 1000)
(hexagonal? 40755 1000)
(hexagonal-and-pentagonal? 40755 1000)
(defn tripentahexagonal? [n]
  (if (hexagonal-and-pentagonal? (triangle n) n)
    n
    false))

(tripentahexagonal? 100000)

(defn tripentahexagonal-indexes-between [min max]
  (filter identity 
    (map tripentahexagonal? (range min max))))

(tripentahexagonal-indexes-between 55000 56000)


;; > 50000 
;; found ! n=55385 => 1533776805
; (triangle 55385)
