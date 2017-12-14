(ns advent-of-code.aoc-2017.day-14
  (:require
    [advent-of-code.aoc-2017.day-10 :refer [knot-hash]]
    [clojure.test :refer [with-test run-tests is are]]
    [clojure.test.check.generators :as gen]))

(def input "amgozmfv")

;(def hex "a0c20170")
; 0xa0c20170
; = 2r10100000110000100000000101110000
; = 2697068912 decimal

(defn ->hash-inputs [input]
  (map
    #(str input "-" %)
    (range 128)))

(defn count-ones [^String hex-32]
  (.bitCount
    (BigInteger. hex-32 16)))


(with-test
  (defn count-empty-squares [input]
   (->> input
        ->hash-inputs
        (map knot-hash)
        (map count-ones)
        (reduce +)))
  #_(is (= 8108 (count-empty-squares "flqrgnkx"))))

(defn solve-part-1 []
  (count-empty-squares input))

(run-tests)

(comment

  (defn random-32-hex []
    (gen/sample (gen/fmap #(apply str %)
                          (gen/vector
                            ;gen/char-alpha
                            (gen/one-of [(gen/choose 0 9)
                                         (gen/fmap
                                           char
                                           (gen/choose 97 102))])
                            32)))))