(ns advent-of-code.aoc-2017.day-03
  (:require
    [clojure.test :refer [with-test run-tests is are]]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [debux.core :refer [dbg]]))


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

(defn point? [p]
  (and
    (= 2 (count p))
    (every? integer? p)))

(defn neighbour? [p1 p2]
  {:pre [(point? p1) (point? p2)]}
  (<= (->> (map - p1 p2)
           (map #(Math/abs %))
           (apply max))
      1))

(defn gen-point-min-distant [dist]
  (gen/such-that
    (fn [point] (->> point (map #(Math/abs %)) (apply max) (<= dist)))
    (gen/vector gen/int 2)))

(defn gen-points-offset-by [gen-diff]
  (gen/let [point (gen/vector gen/int 2)
            diff gen-diff]
           [point (mapv + point diff)]))

(defspec close-neighbours-prop
         100
         (prop/for-all [[p1 p2] (gen-points-offset-by (gen/vector (gen/choose -1 1) 2))]
                       (true? (neighbour? p1 p2))))

(defspec remote-non-neighbours-prop
         100
         (prop/for-all [[p1 p2] (gen-points-offset-by (gen-point-min-distant 2))]
                       (false? (neighbour? p1 p2))))

(def directions
  "Position differences to add to father's coordinates to get get child's."
  [[1 0]
   [0 1]
   [-1 0]
   [0 -1]
   [1 0]])

(defn change-dir? [pos]
  (or (:first pos) (:corner pos)))

(with-test
  (defn ->xy [father-xy dir pos]
    {:xy (mapv + father-xy (nth directions dir))
     :dir (if (change-dir? pos)
            (mod (inc dir) 5)
            dir)})
  (are [father-coord dir pos _ coord] (= coord (->xy father-coord dir pos))
                                      [0 0] 0 #{:first} :-> {:xy [1 0] :dir 1}
                                      [1 0] 1 #{:corner} :-> {:xy [1 1] :dir 2}
                                      [1 1] 2 #{} :-> {:xy [0 1] :dir 2}
                                      [0 1] 2 #{:corner} :-> {:xy [-1 1] :dir 3}
                                      [-1 1] 3 #{} :-> {:xy [-1 0] :dir 3}
                                      [-1 0] 3 #{:corner} :-> {:xy [-1 -1] :dir 4}
                                      [-1 -1] 4 #{} :-> {:xy [0 -1] :dir 4}
                                      [0 -1] 4 #{:corner} :-> {:xy [1 -1] :dir 0}))

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
      (cond-> #{pos}
              first? (conj :first)
              last? (conj :last))))
  (are [loc _ pos] (= pos (position loc))
                   1 :-> #{:corner :first :last}
                   2 :-> #{:next2corner :first}
                   3 :-> #{:corner}
                   4 :-> #{:next2corner}
                   5 :-> #{:corner}
                   6 :-> #{:next2corner}
                   9 :-> #{:corner :last}
                   10 :-> #{:next2corner :first}
                   11 :-> #{:inside}
                   12 :-> #{:next2corner}
                   13 :-> #{:corner}
                   14 :-> #{:next2corner}
                   15 :-> #{:inside}
                   17 :-> #{:corner}
                   21 :-> #{:corner}))

(defn create-next-node
  [prev-square curr-square]
  {:pre [(not-empty prev-square) (every? :sum prev-square)]}
  (let [father (or
                 (last curr-square)
                 (last prev-square))
        loc (inc (:loc father))
        pos (position loc)
        {:keys [xy dir]} (->xy (:xy father) (:dir father) pos)
        neighbours (filter
                     #(neighbour? xy (:xy %))
                     (concat prev-square curr-square))
        sum (reduce
              +
              (map
                :sum
                neighbours))]
    {:loc loc :sum sum :pos pos :xy xy :dir dir}))

(with-test
  (defn make-locations-with-sum
   ([] (let [node {:loc 1 :sum 1 :xy [0 0] :dir 0 :pos #{:corner :0 :1 :2 :3 :first :last}}]
         (cons node
               (lazy-seq
                 (make-locations-with-sum [node] [])))))
   ([prev-square curr-square]
    (let [node (create-next-node prev-square curr-square)
          last? (-> node :pos :last)
          curr-square-updated (conj curr-square node)]
      (cons
        node
        (lazy-seq
          (if last?
            (make-locations-with-sum
              curr-square-updated
              [])
            (make-locations-with-sum
              prev-square
              curr-square-updated)))))))
  (let [nodes (make-locations-with-sum)]
    (are [loc _ sum] (= sum (:sum (nth nodes (dec loc))))
                     1  :-> 1
                     2  :-> 1
                     3  :-> 2
                     4  :-> 4
                     5  :-> 5)))

(defn solve-part-2 [input-num]
  (first
    (drop-while #(<= % input-num)
         (map :sum (make-locations-with-sum)))))

(run-tests)
