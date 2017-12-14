(ns advent-of-code.aoc-2017.day-14
  (:require
    [advent-of-code.aoc-2017.day-10 :refer [knot-hash]]
    [clojure.set :as set]
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

(defn ->knot-hashes [input]
  (->> input
       ->hash-inputs
       (map knot-hash)))

(with-test
  (defn count-empty-squares [input]
   (->> input
        ->knot-hashes
        (map count-ones)
        (reduce +)))
  #_(is (= 8108 (count-empty-squares "flqrgnkx"))))

(defn solve-part-1 []
  (count-empty-squares input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2

;;;;; Find used cells in the memory grid

;(defonce test-knot-hashes (->knot-hashes "flqrgnkx"))

(defn bigint->bool-bits
  "bigint -> seq of booleans, where true=1, false=0; omits leading 0s
  (i.e. if the input is 0, you will get an empty seq back)
  BEWARE: The first element corresponds to the least-significant bit, i.e.
  you might want to revers it so it reads in the same direction as I read decimals,
  left to right"
  [^BigInteger bi]
  (map
    #(.testBit bi %)
    (range (.bitLength bi))))

(defn used-cells-for-hash [^long row ^String hash]
  (->> (BigInteger. hash 16)
       ; "b" => 11
       bigint->bool-bits
       ; => (true true false true)
       (keep-indexed #(when %2 [row %1]))
       (into #{})
       ; => set of [row, column] where the was "1"; ex.: #{[9 0] [9 1] [9 3]}
       ))

(defn find-used-cells [input]
  (->>
    input
    ->knot-hashes
    (map-indexed used-cells-for-hash)
    (reduce set/union))
  )

;;;;;; Region detection

(defn neighbours-in-cross
  "Find neighbouring used cells - look for all neighbours (left/right, up/down) in
  the used-cells set."
  [used-cells cell]
  (let [potentials
        (into #{}
              (map
               #(mapv + cell %)
               [[-1 0] [1 0] [0 -1] [0 1]]))]
    (set/intersection potentials used-cells)))

(defn find-group
  "Detect a group - find all cells that neighbour, directly or through another neighbour, with the given cell"
  ([used-cells cell]
   (when cell
     (find-group used-cells #{} #{cell})))
  ([used-cells group wip-neighbours]
    (if-let [cell (first wip-neighbours)]
      (let [new-neighbours (neighbours-in-cross used-cells cell)]
        (recur
           (set/difference used-cells new-neighbours)
           (conj group cell)
           (set/union
             (disj wip-neighbours cell)
             new-neighbours)))
      group)))

(defn find-all-groups [used-cells]
  (when-let [grp (find-group used-cells (first used-cells))]
    (cons
      grp
      (lazy-seq
        (find-all-groups
          (set/difference used-cells grp))))))

(defn solve-part-2
  "Count the number of regions of neighbouring used cells in the 2D grid
  (only non-diagonal neighbours)"
  []
  (->> input
       find-used-cells
       find-all-groups
       count))

(run-tests)

;(comment
;  (defn random-32-hex []
;    (gen/sample (gen/fmap #(apply str %)
;                          (gen/vector
;                            ;gen/char-alpha
;                            (gen/one-of [(gen/choose 0 9)
;                                         (gen/fmap
;                                           char
;                                           (gen/choose 97 102))])
;                            32)))))