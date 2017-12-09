(ns advent-of-code.aoc-2017.day-09
  (:require
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))

(def input
  (slurp "src/advent_of_code/aoc_2017/day_09_input.txt"))

(defn process-char [state ch]
  (cond
    (:ignore state) (dissoc state :ignore)
    (= ch \!) (assoc state :ignore true)
    (= ch \>) (dissoc state :garbage)
    (:garbage state) (update state :removed (fnil inc 0))
    (= ch \<) (assoc state :garbage true)
    (= ch \{) (update state :depth (fnil inc 0))
    (= ch \}) (-> state
                  (update :sum (fnil + 0) (:depth state))
                  (update :depth dec))
    :else state))

(with-test
  (defn solve-part-1 [input]
    (:sum
      (reduce
       process-char
       {:sum 0}
       (seq input))))
  (are [input _ sum] (= sum (sum-depths input))
                     "{}" :-> 1
                     "{{{}}}" :-> 6
                     "{{},{}}" :-> 5
                     "{{{},{},{{}}}}" :-> 16
                     "{<a>,<a>,<a>,<a>}" :-> 1
                     "{{<ab>},{<ab>},{<ab>},{<ab>}}" :-> 9
                     "{{<!!>},{<!!>},{<!!>},{<!!>}}" :-> 9
                     "{{<a!>},{<a!>},{<a!>},{<ab>}}" :-> 3))

(with-test
  (defn solve-part-2 [input]
    (:removed
      (reduce
        process-char
        {:removed 0}
        (seq input))))
  (are [input _ sum] (= sum (solve-part-2 input))
                     "<>" :-> 0
                     "<random characters>" :-> 17
                     "<<<<>" :-> 3
                     "<{!>}>" :-> 2
                     "<!!>" :-> 0
                     "<!!!>>" :-> 0
                     "<{o\"i!a,<{i<a>" :-> 10))

(run-tests)