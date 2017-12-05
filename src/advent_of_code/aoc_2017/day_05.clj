(ns advent-of-code.aoc-2017.day-05
  (:require [clojure.string :as s])
  (:import (java.util Arrays)))

(def jumps
  (->>
    (slurp "src/advent_of_code/aoc_2017/day_05_input.txt")
    s/split-lines
    (map #(Integer/parseInt %))
    int-array))

#_(def jumps (int-array [0
             3
             0
             1
             -3]))

(defn solve-part-1
  ([] (solve-part-1 0 0))
  ([cnt idx0]
   (let [jump (aget jumps idx0)
         idx (+ idx0 jump)]
     (aset-int jumps idx0 (inc jump))
     ;(println ">>>" cnt idx0 "->" idx "(jump" jump ")" (Arrays/toString jumps))
     (if (< -1 idx (count jumps))
       (recur (inc cnt) idx)
       (inc cnt)))))

(defn solve-part-2
  ([] (solve-part-2 0 0))
  ([cnt idx0]
   (let [jump (aget jumps idx0)
         idx (+ idx0 jump)
         changefn (if (>= jump 3)
                  dec
                  inc)]
     (aset-int jumps idx0 (changefn jump))
     ;(println ">>>" cnt idx0 "->" idx "(jump" jump ")" (Arrays/toString jumps))
     (if (< -1 idx (count jumps))
       (recur (inc cnt) idx)
       (inc cnt)))))