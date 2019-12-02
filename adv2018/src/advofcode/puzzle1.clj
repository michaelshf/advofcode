(ns advofcode.puzzle1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file [file]
  "Loads in the resource file not in resources adds the numbers"
  (mapv clojure.edn/read-string (str/split-lines (slurp file))))
#break
(defn read-from-resource [file]
  (mapv clojure.edn/read-string (str/split-lines (slurp (io/resource file)))))
#break
(def input (read-from-resource "puzzle1.txt"))
(def l-input (cycle input))
(def test-input [3 3 4 -2 -4])
(def l-test-input (cycle test-input))
(def other-test-input (cycle [-6 3 8 5 -6]))

 ;part 1
(defn reducer [file]
  (reduce + (read-from-resource file)))

(reducer "puzzle1.txt")

;; theories:
; create the agg. list by taking looping n sum and conj to list
; - ie: [1 2 3] -> [1] [3] [6] -> [1 3 6]
; then to get which is the first duplicate: loop and distinct
; -once true, grab last
(defn second-freq-finder []
  (loop [index 0
         acc [] ]
    (if (< index (count input))
      (recur (inc index) (conj acc (reduce + (subvec input 0 index))))
      acc)))

; make a vector, add to it the new sums until it's not distinct
; - takes too long
(defn sff2 []
  (loop [index 0
         acc [] ]
    (if (= (count (distinct acc)) (count acc))
      (recur (inc index) (conj acc (reduce + (take index l-test-input))))
      (filter #(< 1 ((frequencies acc) %)) acc))))

; make a set, check if next val is in there, add if not
(defn sff3 [colle]
  (loop [index 0
         acc #{}]
    (if (some #{(reduce + (take index colle))} acc)
      (reduce + (take index colle))
      (recur (inc index) (conj acc (reduce + (take index colle)))))))

; knowing when-let exists:
(defn first-duplicate [numbers]
  (first  ((fn ff [seen xs]
             (lazy-seq
              (when-let [[y & ys] (seq xs)]
                (case (seen y)
                  ::several (ff seen ys)
                  ::once (cons y (ff (assoc seen y ::several) ys))
                  (ff (assoc seen y ::once) ys)))))
           {} numbers)))
