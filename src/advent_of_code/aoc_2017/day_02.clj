(ns advent-of-code.aoc-2017.day-02
  (:require
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMMON
(defn parse-line [line]
  (->> (s/split line #"\t")
       (map #(Integer/parseInt %))))

(defn ->nums [input]
  (->> input
       (s/split-lines)
       (map parse-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 1
(def input-part-1 "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8")

(defn chsum-line [line]
  (->> (sort line)
       ((juxt last first))
       (apply -)))

(defn chsum [nums]
  (reduce
    +
    (map chsum-line nums)))

(with-test
  (defn solve-part-1 [input]
   (chsum (->nums input)))
  (is (= 18 (solve-part-1 input-part-1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2
(def input-part-2 "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5")

(defn find-ratio-in-rest [num nums]
  (some #(when
           (zero? (rem % num))
           (/ % num))
        nums))

(defn find-line-ratio [[n & nums]]
  (if-let [r (find-ratio-in-rest n nums)]
    r
    (recur nums)))

(defn sum [nums]
  (reduce
    +
    (map
      (comp find-line-ratio sort)
      nums)))

(with-test
  (defn solve-part-2 [input]
   (sum (->nums input)))
  (is (= 9 (solve-part-2 input-part-2))))

(run-tests)
