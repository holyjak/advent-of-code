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
;
;(with-test
;  (defn position [loc]
;   (let [subsquares (get-subsquares loc)
;         n (count subsquares)
;         dist-left (- loc (or (last subsquares) 0))
;         side (side-len n)
;         sideways (side-dist side dist-left)
;         max-dist (int (/ side 2))
;         first? (= 1 dist-left)
;         last? (> (count (get-subsquares (inc loc)))
;                  n)
;         pos (case (- max-dist sideways)
;               0 :corner
;               1 :next2corner
;               :inside)]
;     (cond-> #{pos}
;             first? (conj :first)
;             last? (conj :last))))
;  (are [loc _ pos] (= pos (position loc))
;                   1 :-> #{:corner :first :last}
;                   2 :-> #{:next2corner :first}
;                   3 :-> #{:corner}
;                   4 :-> #{:next2corner}
;                   5 :-> #{:corner}
;                   6 :-> #{:next2corner}
;                   9 :-> #{:corner :last}
;                   10 :-> #{:next2corner :first}
;                   11 :-> #{:inside}
;                   12 :-> #{:next2corner}
;                   13 :-> #{:corner}
;                   14 :-> #{:next2corner}
;                   15 :-> #{:inside}
;                   17 :-> #{:corner}
;                   21 :-> #{:corner}))
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
;(with-test
;  (defn inflate-corners
;   "wrap corner elements with nil so we get a ring with the same size as the next one"
;   [prev-ring]
;   (mapcat
;     #(if (:corner (:pos %))
;        [nil % nil]
;        [%])
;     prev-ring))
;  (is (= [] (inflate-corners [])))
;  (is (= [{:pos #{:inside}}] (vec (inflate-corners [{:pos #{:inside}}]))))
;  (is (= [nil {:pos #{:corner}} nil] (vec (inflate-corners [{:pos #{:corner}}])))))
;
;(with-test
;  (defn mk-uncles-seq [prev-ring]
;   (->> prev-ring
;        inflate-corners
;        #_cycle
;        #_(drop (dec (count prev-ring)))
;        #_(partition 3 1)))
;  (let [ring [{:loc 2 :pos #{:next2corner :first}}
;              {:loc 3 :pos #{:corner}}
;              {:loc 4 :pos #{:next2corner}}
;              {:loc 5 :pos #{:corner}}
;              {:loc 6 :pos #{:next2corner}}
;              {:loc 7 :pos #{:corner}}
;              {:loc 8 :pos #{:next2corner}}
;              {:loc 9 :pos #{:corner :last}}
;              ]]
;    (is (empty? (mk-uncles-seq [])))
;    (is (= [nil {:loc 9 :pos #{:corner :last}} {:loc 2 :pos #{:next2corner :first}}] (first (mk-uncles-seq ring))) "#10 neighbours with #9, #2")
;    (is (= [{:loc 9 :pos #{:corner :last}} {:loc 2 :pos #{:next2corner :first}} {:loc 3 :pos #{:corner}}] (second (mk-uncles-seq ring))) "#11 neighbours with #9, #2, #3")
;    (is (= [{:loc 2 :pos #{:next2corner :first}} {:loc 3 :pos #{:corner}} nil] (nth (mk-uncles-seq ring) 2)) "#12 neighbours with #2, #3")
;    (is (= [nil {:loc 3 :pos #{:corner}} nil] (nth (mk-uncles-seq ring) 3)) "#13 neighbours with #3")
;    (is (= [nil {:loc 3 :pos #{:corner}} {:loc 4 :pos #{:next2corner}}] (nth (mk-uncles-seq ring) 4)) "#14 neighbours with #3, #4")
;    (is (= [nil {:loc 9 :pos #{:corner :last}} nil] (nth (mk-uncles-seq ring) 15)) "#25 neighbours with #9")
;    ))
;
;(defn next-node [uncles-seq prev-ring curr-ring]            ;; TODO
;  (let [father (or
;                 (last curr-ring)
;                 (last prev-ring))
;        loc (inc (:loc father))
;        pos (position loc)
;        ring-idx (count curr-ring)
;        brothers (condp apply [pos]
;                   :first []
;                   :last [father (first curr-ring)]
;                   [father])
;        uncles (nth uncles-seq ring-idx)
;        sum (reduce
;              +
;              (map
;                :sum
;                (concat brothers uncles)))]
;    {:loc loc, :sum sum, :pos pos}))
;
;(defn solve-xxx [uncles-seq prev-ring curr-ring]
;  (let [node (next-node uncles-seq prev-ring curr-ring)
;        last? (-> node :pos :last)
;        curr-ring-updated (conj curr-ring node)]
;    (cons
;      node
;      (lazy-seq
;        (if last?
;          (solve-xxx
;            (mk-uncles-seq curr-ring-updated)
;            curr-ring-updated
;            [])
;          (solve-xxx
;            uncles-seq
;            prev-ring
;            curr-ring-updated))))))
;
;

(run-tests)
