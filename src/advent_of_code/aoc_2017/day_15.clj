(ns advent-of-code.aoc-2017.day-15
  (:require
    [clojure.pprint :as pp]))

(def a {:start 277 :factor 16807})
(def b {:start 349 :factor 48271})
(def divisor 2147483647)
(def judgement-len 40000000)

(def bit-mask-16-lowest (.intValue (BigInteger. "0000ffff" 16)))

(defn compute-next [factor last]
  (rem
    (* last factor)
    divisor))

(defn gen [{:keys [factor criteria-mask]} start ]
  (->> (iterate (partial compute-next factor) start)
       rest
       (filter #(if criteria-mask
                  (zero?
                   (bit-and criteria-mask %))
                  true))))

;(defn get-lowest-bits
;  "Get the lowest n bits of a num as a boolean seq (true=1, false=0)"
;  [n num]
;  (map
;    #(bit-test num %)
;    (range n)))
;
;(defn same-lowest-16b? [anum bnum]
;  (= (get-lowest-bits 16 anum)
;     (get-lowest-bits 16 bnum)))


(defn same-lowest-16b? [anum bnum]
  (=
    (bit-and anum bit-mask-16-lowest)
    (bit-and bnum bit-mask-16-lowest)))

(defn solve-part-1 []
  (time
    (let [a-seq (take judgement-len (gen a (:start a)))
          b-seq (take judgement-len (gen b (:start b)))]
     (->> [a-seq b-seq]
          (apply map same-lowest-16b?)
          (filter true?)
          count))))
; duration: 67 s with bit-test of a sequence, 17 s with bit-and

(defn solve-part-2 []
  (time
    (let [a-seq (take 5000000 (gen (assoc a :criteria-mask 2r11 #_4) (:start a)))
          b-seq (take 5000000 (gen (assoc b :criteria-mask 2r111 #_8) (:start b)))]
      (->> [a-seq b-seq]
           (apply map same-lowest-16b?)
           (filter true?)
           count))))