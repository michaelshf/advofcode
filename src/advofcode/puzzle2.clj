(ns advofcode.puzzle2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data :as d]))

;; part one
(def read-file
  (str/split-lines (slurp "resources/puzzle2.txt")))

(def read-test
  (str/split-lines (slurp (io/resource "puzzle2-test.txt"))))

(defn row-doubles? [row]
  (if  (some #(= 2 %) (vals (frequencies row)))
    true
    false))

(defn row-triples? [row]
  (if  (some #(= 3 %) (vals (frequencies row)))
    true
    false))

(defn check-calc [file]
  (let [dubs (count (filter true? (map row-doubles? file)))
        trips (count (filter true? (map row-triples? file)))]
    (* dubs trips)))

(check-calc read-file)


;; part two: which rows differ by one char
(def part2-test
  (str/split-lines (slurp "resources/puzzle2-part2-test.txt")))

(first part2-test)
(second part2-test)
(nth part2-test 4)
(=  (first (second part2-test))
    (first (nth part2-test 4)))

(defn char-diff? [row1 row2 ind]
  (if (not= (nth row1 ind) (nth row2 ind))
    true
    false))

(defn row-diff-by-one? [row1 row2]
  (let [x (range 0 (count row1))]
    (if (= 1 (count (filter true? (map #(char-diff? row1 row2 %) x))))
      true
      false)))

(row-diff-by-one? (second part2-test) (nth part2-test 6))

(defn char-diff [row1 row2 ind]
  (if (not= (nth row1 ind) (nth row2 ind))
    nil
    (nth row1 ind)))

(char-diff (second part2-test) (nth part2-test 4) 0)
(map #(char-diff (second part2-test) (nth part2-test 4) %) (range 0 (count (second part2-test))))

(count  (remove nil? (map #(char-diff (second part2-test) (nth part2-test 4) %) (range 0 (count (second part2-test))))))
(dec (count (second part2-test)))

(defn row-diff-by-one [row1 row2]
  (let [x (range 0 (count row1))]
    (if (= (dec (count row1)) (count (remove nil? (map #(char-diff row1 row2 %) x))))
      (apply str (remove nil? (map #(char-diff row1 row2 %) x)))
      false)))


(row-diff-by-one (second part2-test) (nth part2-test 4))

(defn common-boxes [file]
  (remove false? (for [x1 file x2 file]
                    (row-diff-by-one x1 x2))))

(println  (common-boxes read-file))
