(ns advent-of-code.aoc-2017.day-06
  (:require
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))

(def input (->>
             (s/split
              "0\t5\t10\t0\t11\t14\t13\t4\t11\t8\t8\t7\t1\t4\t12\t11"
              #"\t")
             (mapv #(Integer/parseInt %))))

(defn find-largest
  ([banks]
   (.indexOf
     banks
     (apply max banks))))

(with-test
  (defn redistribute
    ([banks max-idx]
     (redistribute
       (assoc banks max-idx 0)
       (mod (inc max-idx) (count banks))
       (nth banks max-idx)))
    ([banks next-idx val]
     (if (zero? val)
       banks
       (recur
         (update banks next-idx inc)
         (mod (inc next-idx) (count banks))
         (dec val)))))
  (are [banks which-idx _ out-banks] (= out-banks (redistribute banks which-idx))
       [0 2 7 0] 2 :-> [2 4 1 2]
       [2 4 1 2] 1 :-> [3 1 2 3]
       [3 1 2 3] 0 :-> [0 2 3 4]
       [0 2 3 4] 3 :-> [1 3 4 1]
       [1 3 4 1] 2 :-> [2 4 1 2]))

(def history (atom {}))

(defn solve-part-1+2 [banks0 step]
  (swap! history assoc banks0 step)
  (let [largest-idx (find-largest banks0)
        banks (redistribute banks0 largest-idx)]
    (if-let [loop-start (@history banks)]
      {:step (inc step) :loop-len (- (inc step) loop-start)}
      (recur
        banks
        (inc step)))))

;; 7864
(run-tests)