(ns advent-of-code.aoc-2017.day-04
  (:require [clojure.string :as s]))

(def phrases
  (->>
   (slurp "src/advent_of_code/aoc_2017/day_04_input_part1.txt")
   s/split-lines
   (map #(s/split % #"\s+"))))

(defn has-duplicates? [phrase]
  (let [sorted (sort phrase)]
    (not= sorted (dedupe sorted))))

(defn solve-part-1 []
  (count (remove has-duplicates? phrases)))

;(solve-part-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2

(defn has-anagrams? [phrase]
  (let [normalize-words (fn [words] (map (comp s/join sort) words))
        sorted-normalized-words (sort (normalize-words phrase))]
    (not= sorted-normalized-words (dedupe sorted-normalized-words))))

(defn solve-part-2 []
  (count (remove has-anagrams? phrases)))

;(solve-part-2)