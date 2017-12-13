(ns advent-of-code.aoc-2017.day-07
  (:require
    [clojure.string :as s]
    [clojure.set :refer [difference]]
    [clojure.test :refer [with-test run-tests is are]]))

(def line-re #"(\w+) \((\d+)\)(?: -> ((?:\w+,? ?)+))?")

(defn parse-line [line]
  (if-let [[_ name weight children] (re-find line-re line)]
    {:name     name
     :weight      (Integer/parseInt weight)
     :children (when children
                 (s/split children #", "))}))

(def nodes
  (->>
    (slurp "src/advent_of_code/aoc_2017/day_07_input.txt")
    s/split-lines
    (map parse-line)))


(defn solve-part-1 [nodes]
  (let [roots (into #{} (map :name nodes))
        children (into #{} (mapcat :children nodes))]
    (first (difference roots children))))
; => rqwgj

; leaf parents: all children must have the same weight
; inner parents: (child + weight of its children) must be same for all
; exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?

(def nodes-map (->> nodes
                    (map (juxt :name identity))
                    (into {})))

; I need:
; 1) correct total (majority)
; 2) weight of the culprit (its total - sum of children) <=== its weight
(with-test
  (defn get-correct-weight [weights]
   (let [[correct-total cnt] (apply max-key
                                    second
                                    (frequencies
                                      (map :total weights)))
         {:keys [total child-totals]} (some #(when (not= (:total %) correct-total)
                                   %) weights)
         w (- total (apply + child-totals))]
     (- w (- total correct-total))))
  (is (= 333 (get-correct-weight [{:total 1398 :child-totals [191 191 191 191 191 191 191]}
                                  {:total 1398 :child-totals [196 196 196]}
                                  {:total 1398 :child-totals [240 240 240]}
                                  {:total 1398 :child-totals [167 167 167]}
                                  {:total 1406 :child-totals [213 213 213 213 213]}]))))

(def test-nodes
  {:0 {:weight 100 :children [:01 :02 :03]}
   :01 {:weight 5 :children nil}
   :02 {:weight 3 :children [:02.01 :02.02]}
   :02.01 {:weight 1 :children nil}
   :02.02 {:weight 1 :children nil}
   :03 {:weight 2 :children [:03.01 :03.02 :03.03]}
   :03.01 {:weight 1 :children nil}
   :03.02 {:weight 1 :children nil}
   :03.03 {:weight 1 :children nil}
   })

(with-test
  (defn check-balance
    "Find the program (node) with the wrong weight

    ASSUMPTION: The program with the wrong weight has at least 2 siblings
    (so that it is easy to see which one is the incorrect one). If that doesn't
    hold then implement a smarter algorithm.

    NOTE: I cheat here and use an exception to communicate the return value as it
    makes the flow control simpler :-)
    "
    [nodes-map node]
   (let [chs (map nodes-map (:children node))
         ws (map (partial check-balance nodes-map) chs)
         totals (map :total ws)
         balanced? (if (seq totals)
               (apply = totals)
               0)
         sum (apply + totals)]
     (if balanced?
       {:total (+ (:weight node) sum) :child-totals totals}
       (if (= 2 (count chs))
         (throw (ex-info "cannot determine the imbalanced child for I only have 2" {:n node :chs chs}))
         (throw (ex-info "ANSWER", {:correct-weight (get-correct-weight ws)}))))))
  (is (= {:total 100 :child-totals []} (check-balance {} {:weight 100 :children nil})))
  (is (= {:total 115 :child-totals [5 5 5]} (check-balance test-nodes (:0 test-nodes)))))

(defn solve-part-2 []
  (check-balance nodes-map (nodes-map "rqwgj")))

(run-tests)