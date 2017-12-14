(ns advent-of-code.aoc-2017.day-10
  (:require
    [debux.core :refer [dbg]]
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))

; This hash function simulates tying a knot in a circle of string with 256 marks on it. Based on the input to be hashed, the function repeatedly selects a span of string, brings the ends together, and gives the span a half-twist to reverse the order of the marks within it. After doing this many times, the order of the marks is used to build the resulting hash.
; To achieve this, begin with a list of numbers from 0 to 255, a current position
; which begins at 0 (the first element in the list), a skip size (which starts at 0),
; and a sequence of lengths (your puzzle input). Then, for each length:
; - Reverse the order of that length of elements in the list, starting with the element at the current position.
; - Move the current position forward by that length plus the skip size.
; - Increase the skip size by one.
; Q: what is the result of multiplying the first two numbers in the list?

(with-test
  (defn double-split-cycled [start len coll]
   {:pre [(integer? start) (integer? len) (< start (count coll))]
    :pos [(= (count coll) (count (concat (:beg %) (:middle %) (:end %))))]}
   (let [[beginning0 end0] (split-at start coll)
         [middle-start end] (split-at len end0)
         overflow-len (- len (count middle-start))
         [middle-end beginning] (split-at overflow-len beginning0)]
     {:beg      beginning
      :end      end
      :middle   (concat middle-start middle-end)
      :overflow overflow-len}))
  (are [start len _ res] (= res
                            ((juxt :beg :middle :end :overflow)
                              (double-split-cycled start len (range 5))))
                     0 3 :-> [[] [0 1 2] [3 4] 0]
                     1 3 :-> [[0] [1 2 3] [4] 0]
                     2 3 :-> [[0 1] [2 3 4] [] 0]
                     3 3 :-> [[1 2] [3 4 0] [] 1]
                     3 5 :-> [[] [3 4 0 1 2] [] 3]))

(def input "183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88")

(def lengths
  (->>
    (s/split input #",")
    (map #(Integer/parseInt %))))

(defn split-at-last [n coll]
  (split-at
    (- (count coll) n)
    coll))

(with-test
  (defn consume-length [{string0 :string
                         :keys [pos skip]
                         :or   {string0 (range 256) pos 0 skip 0}}
                       len]
   {:pre [(<= len (count string0))] :post [(= (count string0) (count (:string %)))]}
    (let [max (count string0)
          {:keys [beg middle end overflow]} (double-split-cycled pos len string0)
          rev (reverse middle)
          [rev-end rev-beg] (split-at-last overflow rev)
          string (concat rev-beg beg rev-end end)]
     {:string string
      :pos    (mod
                (+ pos len skip)
                max)
      :skip   (inc skip)}))
  (is (= [2 1 0 3 4] (:string (consume-length {:string (range 5)} 3))))
  (is (= [4 3 0 1 2] (:string (consume-length {:string [2 1 0 3 4] :skip 1 :pos 3} 4))))
  (is (= [4 3 0 1 2] (:string (consume-length {:string [4 3 0 1 2] :skip 2 :pos 4} 1))))
  (is (= [3 4 2 1 0] (:string (consume-length {:string [4 3 0 1 2] :skip 3 :pos 1} 5)))))

(defn round [params lengths]
  (reduce
    consume-length
    params
    lengths))

(with-test
  (defn solve-part-1
    ([] (solve-part-1 lengths {}))
    ([lengths params]
      (->> (round params lengths)
          :string
          (take 2)
          (apply *))))
  (is (= 12 (solve-part-1 [3, 4, 1, 5] {:string [0, 1, 2, 3, 4]}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2

(def default-last-lengths [17, 31, 73, 47, 23])

(defn ->dense-hash [sparse-hash]
  (let [blocks (partition 16 sparse-hash)]
    (map
      #(apply bit-xor %)
      blocks)))

(defn str->lengths [string]
  (->> (seq string)
       (map int)))

(with-test
  (defn solve-part-2
   ([] (solve-part-2 input {}))
   ([input] (solve-part-2 input {}))
   ([input params]
    (let [lengths (concat (str->lengths input) default-last-lengths)
          sparse-hash
          (:string
            (reduce
              (fn [params _] (round params lengths))
              params
              (range 64)))]
      (->> sparse-hash
           (->dense-hash)
           (map (partial format "%02x"))
           (s/join)))))
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (solve-part-2 "")))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd" (solve-part-2 "AoC 2017")))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (solve-part-2 "1,2,3")))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (solve-part-2 "1,2,4"))))

(defn knot-hash [^String input]
  (solve-part-2 input))

(run-tests)