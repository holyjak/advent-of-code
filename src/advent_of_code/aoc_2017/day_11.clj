(ns advent-of-code.aoc-2017.day-11
  (:require
    [debux.core :refer [dbg]]
    [clojure.string :as s]
    [clojure.test :refer [with-test run-tests is are]]))

(def dirs (map
            keyword
            (-> (slurp "src/advent_of_code/aoc_2017/day_11_input.txt")
               (s/replace "\n" "")
               (s/split #","))))

(def opposite-dirs [[:s :n]
                    [:se :nw]
                    [:sw :ne]])

(defn init [dirs]
  (merge
    (zipmap [:n :ne :se :s :sw :nw ] (repeat 0))
    dirs))

(defn freq->counts [freq directions]
  (map freq directions))

(with-test
  (defn pair-diff-abs [freq opposites]
    {:pre [(map? freq) (seq opposites) (= 2 (count opposites))]}
   (->> opposites
        (freq->counts freq)
        (apply min)))
  (is (= 2 (pair-diff-abs {:n 3 :s 2} [:n :s])))
  (is (= 8 (pair-diff-abs {:n 10 :s 8} [:n :s])))
  (is (= 8 (pair-diff-abs {:n 8 :s 10} [:n :s])))
  (is (= 10 (pair-diff-abs {:nw 10 :se 30} [:nw :se]))))


(defn add-to-dirs [freq val dirs]
  (reduce #(update %1 %2 + val) freq dirs))

(with-test
  (defn annihilate-opposites [freq]
    {:pre [(map? freq)]}
   (reduce
     (fn [freq dir]
       (add-to-dirs
         freq
         (- (pair-diff-abs freq dir))
         dir))
     freq
     opposite-dirs))
  (is (= (init {:s 2 :n 0 :ne 0 :sw 9}) (annihilate-opposites (init {:s 10 :n 8 :ne 1 :sw 10})))))

(with-test
  (defn substitute-in-quadrant [freq quadrant]
   (let [corner-dir (s/join quadrant)
         intersection (apply min
                             (freq->counts
                               freq
                               quadrant))]
     (-> (add-to-dirs freq (- intersection) quadrant)
         (update corner-dir + intersection))))
  ;(is (= (init {:n 1 "e" 0 :ne 12}) (substitute-in-quadrant (init {:n 3 "e" 2 :ne 10}) [:n "e"])))
  ;(is (= (init {:n 1 "e" 0 :ne 12}) (substitute-in-quadrant (init {:n 3 "e" 2 :ne 10}) [:n "e"])))
  )

(defn dir-intersection [freq dirs]
  (apply min
         (freq->counts freq dirs)))

(defn ->main-dir
  "Get the mai ndirection of a side direction; ex.: :nw -> :n, :se -> :s"
  [dir]
  (-> dir
      name
      (subs 0 1)
      keyword))

(with-test
  (defn optimize-pole-dirs
    "Replace [:ne :nw] with a single [:n] step."
    [freq]
    (reduce
      (fn [freq changes] (merge-with + freq changes))
      freq
      (for [pole-side-dirs [[:ne :nw] [:se :sw]]]
       (let [intersection (dir-intersection freq pole-side-dirs)]
         (->
           (zipmap pole-side-dirs (repeat (- intersection)))
           (assoc
             (->main-dir (first pole-side-dirs))
             intersection))))))
  (is (= (init {:ne 1 :nw 0 :n 2}) (optimize-pole-dirs (init {:ne 3 :nw 2})))))

(with-test
  (defn swap [freq [from-dir to-dir] main-dir]
   (let [max-swap (min
                    (freq main-dir)
                    (freq from-dir))]
     (merge-with
       +
       freq
       {main-dir (- max-swap)
       from-dir (- max-swap)
       to-dir   (+ max-swap)})))
  #_(are [freq dirs main _ exp] (= exp (swap freq dirs main))
                              (init {}) [:se :ne] :n :-> (init {:se 0 :ne 0 :n 0})
                              (init {:n 1 :se 1}) [:se :ne] :n :-> (init {:se 0 :ne +1 :n 0})
                              (init {:n 3 :se 1}) [:se :ne] :n :-> (init {:se 0 :ne +1 :n 2})
                              (init {:n 1 :se 3}) [:se :ne] :n :-> (init {:se 2 :ne +1 :n 0})
                              (init {:n 3 :se 3}) [:se :ne] :n :-> (init {:se 0 :ne +3 :n 0})))

(defn swap-all [freq main-dir pairs]
  (reduce
    #(swap %1 %2 main-dir)
    freq
    pairs))

(with-test
  (defn optimize-side-dirs
    "Make n/w shorter by going on an 'away' diagonal instead of n/s and then going on a 'back'
     diagonal. Ex (away = n, back = s): [n nw nw sw sw] -> [nw nw nw sw]"
    [freq]
    (if (> (:n freq) 0)
      (swap-all freq :n [[:se :ne] [:sw :nw]] )
      (swap-all freq :s [[:ne :se] [:nw :sw]] ))))

(with-test
  (defn solve-part-1
    ([] (solve-part-1 dirs))
    ([dirs]
      ; FIXME add
      (let [freq (init (frequencies dirs))]
        (->> freq
             annihilate-opposites
             optimize-pole-dirs
             optimize-side-dirs
             vals
             (apply +)
             ))))
  (is (= 2 (solve-part-1 [:n :n :n :s]))))

(defn solve-part-2
  "Very inefficient, very simple"
  []
  (let [all-subdirs (map #(take % dirs) (range (count dirs)))]
    (reduce
     max
     (map solve-part-1 all-subdirs))))


(run-tests)