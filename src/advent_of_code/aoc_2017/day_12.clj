(ns advent-of-code.aoc-2017.day-12
  (:require
    [debux.core :refer [dbg]]
    [clojure.set :as set]
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))

(def line-re #"(\d+) \<-\> ((?:\d+,? ?)+)")

(defn parse-line [line]
  (if-let [[_ id neighbours] (re-find line-re line)]
    [(Integer/parseInt id)
     (->> (s/split neighbours #", ")
          (map #(Integer/parseInt %))
          (into #{}))]))

(def links
  (->>
    (slurp "src/advent_of_code/aoc_2017/day_12_input.txt")
    s/split-lines
    (map parse-line)
    (into {})))

(defn discover-group [lookup group wip-nodes]
  (let [[node children] (find lookup (first wip-nodes))
       new-children (set/difference children group)]
    #_(dbg [group wip-nodes node new-children (count lookup)])
    (if-not node
      {:group group :lookup lookup}
      (recur
        (dissoc lookup node)
        (set/union group new-children)
        (set/union
         (disj wip-nodes node)
         new-children)))))

(defn solve-part-1 []
  (count
    (:group
      (discover-group links #{0} #{0}))))

(defn discover-all-groups [groups lookup0]
  (let [[node _] (first lookup0)
        {:keys [group lookup]} (discover-group lookup0 #{node} #{node})]
    (if-not node
      groups
      (recur
        (conj groups group)
        lookup))))

(defn solve-part-2 []
  (count (discover-all-groups [] links)))

(run-tests)
