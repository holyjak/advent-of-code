(ns advent-of-code.aoc-2017.day-08
  (:require
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))

(def line-re #"^(\w+) (inc|dec) (-?\d+) if (\w+) ([<>=!]{1,2}) (-?\d+)$")

(defn ->clojure-pred [pred]
  (or
    ({">" >
      "<" <
      ">=" >=
      "<=" <=
      "!=" not=
      "==" =}
      pred)
    (throw (IllegalArgumentException. (str "Unsupported predicate" pred)))))


(defn get-reg [regs name]
  (or (regs name) 0))

(defn set-reg! [regs name op val]
  {:pre [(string? name) (#{+ -} op) (integer? val)]}
  (update regs name (fnil op 0) val))

(with-test
  (defn parse-line [^String line]
   (if-let [[_ reg1 op n1 reg2 pred n2] (re-find line-re line)]
     [(fn [regs] (set-reg!
                   regs
                   reg1
                   ({"inc" +, "dec" -} op)
                   (Integer/parseInt n1)))
      (fn [regs] ((->clojure-pred pred)
                   (get-reg regs reg2)
                   (Integer/parseInt n2)))]
     (throw (IllegalArgumentException. (str "unparseable line: " line)))))
  (is (= [{"xx" -1, "cc" 1} true]
         (let [regs {"cc" 1}] (map #(% regs)
                             (parse-line "xx dec 1 if cc == 1")))))
  (is (= [{"b" -5} true]
         (let [regs {}] (map #(% regs)
                             (parse-line "b inc -5 if a > -1"))))))

(def instructions
  (->>
    (slurp "src/advent_of_code/aoc_2017/day_08_input.txt")
    s/split-lines
    (map parse-line)))

(defn exec [regs [instr cond]]
  (if (cond regs)
    (instr regs)
    regs))

(defn max-reg-val [regs]
  (reduce max (vals regs)))

(defn exec-and-update-max [max-atom regs0 instr]
  (let [regs (exec regs0 instr)
        current-max (max-reg-val regs)]
    (swap! max-atom max current-max)
    regs))

(defn exec-all [instrs]
  (let [total-max (atom 0)
        regs (reduce
               (partial exec-and-update-max total-max)        ;exec
               {}
               instrs)]
    {:final-max (max-reg-val regs) :total-max-encountered @total-max}))

(defn solve-part-1_2 []
  (exec-all instructions))

(run-tests)