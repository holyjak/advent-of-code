(ns advent-of-code.aoc-2017.day-13
  (:require
    [debux.core :refer [dbg]]
    [clojure.set :as set]
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))


(def line-re #"(\d+): (\d+)")

(defn parse-line [line]
  (let [[_ depth range] (re-find line-re line)]
    (mapv #(Integer/parseInt %) [depth range])))

(def firewall
  (->>
    (slurp "src/advent_of_code/aoc_2017/day_13_input.txt")
    s/split-lines
    (map parse-line)
    (into {})))

(with-test
  (defn caught-upon-leaving
    "Is the scanner at position 0 of its `range` when I leave it at time `t`?
    (I leave depth 0 at time 0, depth N at time N. All scanners start moving from position 0 at time 0.)"
    [range t]
   (zero?
     (mod
       t
       (max (- (* 2 range) 2) 1))))
  (is (true? (caught-upon-leaving 1 0)))
  (is (true? (caught-upon-leaving 2 0)))
  (is (false? (caught-upon-leaving 2 1)))
  (is (true? (caught-upon-leaving 2 2)))
  (is (true? (caught-upon-leaving 3 0)))
  (is (false? (caught-upon-leaving 3 1)))
  (is (false? (caught-upon-leaving 3 2)))
  (is (false? (caught-upon-leaving 3 3)))
  (is (true? (caught-upon-leaving 3 4)))
  (is (false? (caught-upon-leaving 3 5))))

(defn severity-of-catch [depth range]
  (* depth range))

(defn severity-at-time [firewall depth-and-time]
  (if-let [range (firewall depth-and-time)]
    (if (caught-within range depth-and-time)
      (severity-of-catch depth-and-time range)
      0)
    0))

(defn trip-severity [firewall]
  (reduce
    (fn [sum depth-and-time]
      (+ sum (severity-at-time firewall depth-and-time)))
    0
    (keys firewall)))

(defn solve-part-1 []
  (trip-severity firewall))

(run-tests)