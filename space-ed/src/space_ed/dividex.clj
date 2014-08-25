(ns space-ed.dividex)

(defn dividex
  "return the sequence of all x divisions of the unit interval"
  [x] (map #(/ % x) (range x)))

(defn merge-patterns [p1 p2]
  (loop [rem1 p1
         rem2 p2
         outseq []]
    (cond
      (empty? rem1)  (into outseq rem2)
      (empty? rem2)  (into outseq rem1)
      (= (first rem1) (first rem2)) (recur (rest rem1) (rest rem2) (conj outseq (first rem1)))
      (< (first rem1) (first rem2)) (recur (rest rem1) rem2 (conj outseq (first rem1)))
      (> (first rem1) (first rem2)) (recur rem1 (rest rem2) (conj outseq (first rem2)))
      )
    )
  )

(defn diff-patterns [p1 p2]
  (seq (clojure.set/difference (set p1) (set p2))))

(defn exclude-patterns [p1 p2]
  (seq (clojure.set/union (diff-patterns p1 p2) (diff-patterns p2 p1))))

(defn int-patterns [p1 p2]
  (seq (clojure.set/difference (set p1) (set p2))))


(def dividex-32 (vec (for [i (range 32)] (dividex i))))

(def dvx-3+5 (merge-patterns (dividex-32 3)(dividex-32 5)))
