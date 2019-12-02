(ns advofcode.puzzle3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-input
  (str/split-lines (slurp (io/resource "puzzle3-test.txt"))))

(def input
  (str/split-lines (slurp (io/resource "puzzle3.txt"))))

(defn parse-claim [claim]
  "takes a claim, parses out the claim # (0th), start (1st) and area (2nd)"
  (vector (re-find #"\d+" claim)         
          (re-find #"\d+[,]\d+" claim)   
          (re-find #"\d+[x]\d+" claim)))

(defn claim-coords [claim]
  "takes a claim, returns a set of vec'd coordinates the claim emcompasses"
  (let [xstart (Integer/parseInt (re-find #"\d+" (second (parse-claim claim))))
        ystart (Integer/parseInt (subs (re-find #",\d+" (second (parse-claim claim))) 1))
        xend (dec (+ xstart (Integer/parseInt (re-find #"\d+" (nth (parse-claim claim) 2)))))
        yend (dec (+ ystart (Integer/parseInt (subs (re-find #"[x]\d+" (nth (parse-claim claim) 2)) 1))))]
    (set (for [x (apply vector (range xstart (inc xend)))
                y (apply vector (range ystart (inc yend)))]
            [x y]))))


