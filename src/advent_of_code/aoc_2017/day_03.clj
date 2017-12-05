(ns advent-of-code.aoc-2017.day-03
  (:require
    [clojure.test :refer [with-test run-tests is are]]))
;
;You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
;
;Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
;
;17  16  15  14  13
;18   5   4   3  12
;19   6   1   2  11
;20   7   8   9  10
;21  22  23---> ...
;While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
;
;For example:
;
;Data from square 1 is carried 0 steps, since it's at the access port.
;Data from square 12 is carried 3 steps, such as: down, left, left.
;Data from square 23 is carried only 2 steps: up twice.
;Data from square 1024 must be carried 31 steps.
;How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

(defn side-len [n]
  (+ 1
     (* 2 n)))

(def squares (map #(int (Math/pow
                          (side-len %)
                          2))
                  (range)))

(defn get-subsquares [loc]
  (take-while
    (partial > loc)
    squares))

(with-test
  (defn side-dist [side loc]
    (let [d (mod loc (max (dec side) 1))
          max-dist (int (/ side 2))]
      (apply
        min
        [(Math/abs (- max-dist d))
         (Math/abs (- d max-dist))])))
  (is (= 0 (side-dist 1 1)))
  (is (= 1 (side-dist 5 (- 12 9))))
  (is (= 2 (side-dist 5 (- 13 9))))
  (is (= 1 (side-dist 5 (- 14 9))))
  (is (= 0 (side-dist 5 (- 15 9))))
  (is (= 1 (side-dist 5 (- 16 9))))
  (are [loc _ sideways] (= sideways (side-dist 5 loc))
                        ; max dist: 5/2 = 2
                        (- 10 9) :->  1
                        (- 11 9) :->  0
                        (- 12 9) :->  1
                        (- 13 9) :->  2
                        (- 14 9) :->  1                     ; got 2
                        (- 15 9) :->  0                     ; got 1
                        (- 16 9) :->  1                     ; got 0
                        (- 17 9) :->  2                     ; got 1
                        (- 18 9) :->  1                     ; got 2
                        (- 19 9) :->  0
                        (- 20 9) :->  1
                        )
  (are [loc _ sideways] (= sideways (side-dist 7 loc))
       ; max dist: 7/2 = 3
       1 :->  2
       2 :->  1
       3 :->  0
       4 :->  1
       5 :->  2
       6 :->  3
       7 :->  2
       8 :->  1))

(with-test
  (defn solve-part-1 [loc]
    (let [subsquares (get-subsquares loc)
          n (count subsquares)
          dist-left (- loc (or (last subsquares) 1))
          away n
          side (side-len n)
          sideways (side-dist side dist-left)]
      (if (zero? dist-left)
        0
        (+ away sideways))))
  (is (= 0 (solve-part-1 1)))
  (is (= 3 (solve-part-1 12)))
  (is (= 2 (solve-part-1 9)))
  (is (= (+ 2 2) (solve-part-1 25)))
  (is (= (+ 3 2) (solve-part-1 26)))
  (is (= (+ 3 1) (solve-part-1 27)))
  (is (= (+ 3 0) (solve-part-1 28)))
  (is (= (+ 3 1) (solve-part-1 29)))
  (is (= (+ 3 2) (solve-part-1 30)))
  (is (= 5 (solve-part-1 26)))
  (is (= 31 (solve-part-1 1024))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2
(defn- ->kwd [num]
  (keyword (str num)))

(with-test
  (defn- side-nr
    "Side this index is on (0 ... 3). Index 0 is at the initial corner of side 0,
     i.e. the first location in each square has index 1 (as it is next to the corner).
     Each side has (len - 1) elements (corners belong to the next side)"
    [side-len zero-based-index]
   (int (mod
          (/ zero-based-index
             (max (dec side-len) 1))                                ;; because corner is shared by 2 sides
          4)))
  (are [side idx _ nr] (= nr (side-nr side idx))
                       3 0 :-> 0
                       3 1 :-> 0
                       3 2 :-> 1
                       3 3 :-> 1
                       3 4 :-> 2
                       3 5 :-> 2
                       3 6 :-> 3
                       3 7 :-> 3
                       3 8 :-> 0                            ;; wrap around
                       5 1 #_10 :-> 0
                       5 2 #_11 :-> 0
                       5 4 #_13 :-> 1
                       5 7 #_16 :-> 1
                       5 8 #_17 :-> 2
                       5 16 #_25 :-> 0
                       ))

(with-test
  (defn position
    "
    Derive node position information for the given location.
    It contains
    - the side(s) (:0 .. :3, corners have 2)
    - relation to corners - one of :corner, :next2corner, :inside.
    - potentially :first or :last (withing the given square, i.e. spiral's loop)
    "
    [loc]
   (let [subsquares (get-subsquares loc)
         n (count subsquares)
         dist-left (- loc (or (last subsquares) 0))
         side (side-len n)
         side-n (side-nr side dist-left)                        ; side = 0 ... 3
         side-n-prev (side-nr side (dec dist-left))                        ; side = 0 ... 3
         sideways (side-dist side dist-left)
         max-dist (int (/ side 2))
         first? (= 1 dist-left)
         last? (> (count (get-subsquares (inc loc)))
                  n)
         ;before-corner? (:corner (position (inc loc)))
         pos (case (- max-dist sideways)
               0 :corner
               1 :next2corner
               :inside)]
     (cond-> #{pos (keyword (str side-n))}
             (= :corner pos) (conj (->kwd side-n-prev))
             (= 1 loc) (conj :0 :1 :2 :3)
             first? (conj :first)
             last? (conj :last))))
  (are [loc _ pos] (= pos (position loc))
                   1 :-> #{:corner :first :last :0 :1 :2 :3}
                   2 :-> #{:next2corner :first :0}
                   3 :-> #{:corner :0 :1}
                   4 :-> #{:next2corner :1}
                   5 :-> #{:corner :1 :2}
                   6 :-> #{:next2corner :2}
                   9 :-> #{:corner :last :3 :0}
                   10 :-> #{:next2corner :first :0}
                   11 :-> #{:inside :0}
                   12 :-> #{:next2corner :0}
                   13 :-> #{:corner :0 :1}
                   14 :-> #{:next2corner :1}
                   15 :-> #{:inside :1}
                   17 :-> #{:corner :1 :2}
                   21 :-> #{:corner :2 :3}))
;
;(with-test
;  (defn make-sums
;   ([]
;    (let [root {:loc 1, :pos :corner, :sum 1}]
;      (comment lazy-seq
;        (cons
;          root
;          (make-sums root [root] 0)))))
;   ([father sums tail-loc]
;    (let [tail (drop tail-loc sums)
;          ;; FIXME IGNORE FATHER FOR 1ST NODE
;          ;; FIXME LAST NODE HAS THE 1ST AS NEIGHBOR TOO
;          end (first tail)
;          loc (inc (:loc father))
;          pos (position loc)
;          sub-neighbors (take
;                      (comment cond
;                        (pos :corner) 1
;                        (pos :next2corner) 2
;                        3)
;                      tail)
;          eq-neighbors (cond-> []
;                               (not (pos :first)) (conj father)
;                               (pos :last) :TODO-add-first)
;          sum (reduce
;                +
;                (:sum father)
;                (map :sum sub-neighbors))
;          child {:loc loc, :pos pos, :sum sum}
;          drop-cnt (if (and
;                         (= (:pos end) :corner)
;                         (#{:corner :next2corner} pos))
;                     0
;                     1)]
;      (lazy-seq
;        (cons
;          child
;          (make-sums
;            child
;            (cons child sums)
;            (+ tail-loc drop-cnt)))))))
;  ;(is (= 1 (:sum (last (take 1 (make-sums))))))
;  ;(is (= 1 (:sum (last (take 2 (make-sums))))))
;  ;(is (= 2 (:sum (last (take 3 (make-sums))))))
;  ;(is (= 4 (:sum (last (take 4 (make-sums))))))
;  ;(is (= 5 (:sum (last (take 5 (make-sums))))))
;  )
;

(def test-data-squares
  [[{:loc 1 :pos #{:corner :first :last :0 :1 :2 :3}}]
   [{:loc 2 :pos #{:next2corner :first :0}}
    {:loc 3 :pos #{:corner :0 :1}}
    {:loc 4 :pos #{:next2corner :1}}
    {:loc 5 :pos #{:corner :1 :2}}
    {:loc 6 :pos #{:next2corner :2}}
    {:loc 7 :pos #{:corner :2 :3}}
    {:loc 8 :pos #{:next2corner :3}}
    {:loc 9 :pos #{:corner :last :0 :3}}]])

(with-test
  (defn- ->prev-square-idx
    "Map the index in square N to the index of the closest neighbour in square N-1.
    (The first location in a square (i.e. 1, 2, 10, 26, ...) has the idx 0.)
    "
    [prev-square pos idx]
   (let [curr-ring-len (+ (count prev-square) 8)
         side-len (/ curr-ring-len 4)
         side (int (/ idx side-len))                        ; side = 0 ... 3
         corner? (:corner pos)
         corner-subtractions (* side 2)]
     (- idx
        corner-subtractions
        (if corner?
          1
          0))))
  (let [corner #{:corner}
        next2corner #{:next2corner}
        inside #{:inside}
        square (->> (nth test-data-squares 1)
                 ((juxt last butlast))
                 (apply cons)
                  cycle)]
    (are [idx pos _ res] (= res (select-keys (nth square (->prev-square-idx (range 0 8) pos idx)) [:loc]))
                          #_10 0 next2corner :-> {:loc 9}
                          #_11 1 inside :-> {:loc 2}
                          #_12 2 next2corner :-> {:loc 3}
                          #_13 3 corner :-> {:loc 3}
                          #_14 4 next2corner :-> {:loc 3}
                          #_15 5 inside :-> {:loc 4}
                          #_23 13 inside :-> {:loc 8}
                          #_24 14 next2corner :-> {:loc 9}
                          #_25 15 corner :-> {:loc 9}
                          )))

(defn- same-side-nodes [pos nodes]
  (let [side-id (some identity
                      ((juxt :0 :1 :2 :3) pos))]
    (filter
      (fn same-side
        [node]
        ((:pos node) side-id))
      ; Remove duplicates needed due to the square that only has [{:loc 1}]
      (dedupe nodes))))

(defn ->locs [nodes]
  (map #(select-keys % [:loc]) nodes))

(with-test
  (defn prev-square-neighbors
    "Return all relevant neighbours from the previous square.
    (The first location in a square (i.e. 1, 2, 10, 26, ...) has the idx 0.)
    "
    [prev-square pos idx]
   (let [prev-idx (->prev-square-idx prev-square pos idx)
         prev-square-len (count prev-square)
         nodes (drop
                 ;; offset so that e.g loc 9 is the first (as it neighbours loc 10)
                 (dec prev-square-len)
                 (cycle prev-square))
         ; A node may neighbour with up to 3 nodes in the previous ring:
         max-neighbours (cond
                          (:first pos) 2
                          :else 3)
         triplet (take max-neighbours
                       (drop
                         (dec prev-idx)
                         nodes))]
     (if (:corner pos)
       [(nth triplet 1)]                                    ; corners have only 1 neighbour
       (same-side-nodes pos triplet))))
  (let [->pos (fn [& kwds] (apply conj #{} kwds))
        prev-square (second test-data-squares)]
    (is (= [{:loc 1}] (->locs (prev-square-neighbors
                         (first test-data-squares)
                         #{:next2corner :1}
                         2 #_loc_4))))
    (are [idx pos _ res] (= res (->locs
                                   (prev-square-neighbors prev-square pos idx)))
                          #_10 0 (->pos :next2corner :0 :first)  :-> [{:loc 9} {:loc 2}]
                          #_11 1 (->pos :inside :0)       :-> [{:loc 9} {:loc 2} {:loc 3}]
                          #_12 2 (->pos :next2corner :0)  :-> [{:loc 2} {:loc 3}]
                          #_13 3 (->pos :corner :0 :1)    :-> [{:loc 3}]
                          #_14 4 (->pos :next2corner :1)  :-> [{:loc 3} {:loc 4}]
                          #_15 5 (->pos :inside :1)       :-> [{:loc 3} {:loc 4} {:loc 5}]
                          #_23 13 (->pos :inside :3)      :-> [{:loc 7} {:loc 8} {:loc 9}]
                          #_24 14 (->pos :next2corner :3) :-> [{:loc 8} {:loc 9}]
                          #_25 15 (->pos :corner :0 :3)   :-> [{:loc 9}]
                          )))


(defn create-next-node ;; TODO
  [prev-square curr-square]
  {:pre (not-empty prev-square)}
  (let [father (or
                 (last curr-square)
                 (last prev-square))
        loc (inc (:loc father))
        pos (position loc)
        idx (inc (count curr-square))
        uncles (prev-square-neighbors prev-square pos idx)
        ;; FIXME in square n=1,s=3 we can reach brothers on the diagonal too
        brothers (condp apply [pos]
                   :first []
                   :last [father (first curr-square)]
                   [father])
        _ (println ">>> create-next-node loc:" loc
                   pos
                   "BRO>" brothers
                   "UNC>" uncles)                           ;; FIXME rm
        sum (reduce
              +
              (map
                :sum
                (concat brothers uncles)))]
    {:loc loc, :sum sum, :pos pos}))

(with-test
  (defn solve-xxx
   ([] (let [node {:loc 1 :sum 1 :pos #{:corner :0 :1 :2 :3 :first :last}}]
         (cons node
               (lazy-seq
                 (solve-xxx [node] [])))))
   ([prev-square curr-square]
    (let [node (create-next-node prev-square curr-square)
          last? (-> node :pos :last)
          curr-square-updated (conj curr-square node)]
      (cons
        node
        (lazy-seq
          (if last?
            (solve-xxx
              curr-square-updated
              [])
            (solve-xxx
              prev-square
              curr-square-updated)))))))
  (let [nodes (solve-xxx)]
    (are [loc _ sum] (= sum (:sum (nth nodes (dec loc))))
                     1  :-> 1
                     2  :-> 1
                     3  :-> 2
                     4  :-> 4
                     5  :-> 5
                     )))



(run-tests)
