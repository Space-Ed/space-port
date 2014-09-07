(ns space-ed.genode
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set :as set]))

(defprotocol Extracted
  (extract [this ext-params]))

(extend-type nil
  Extracted
  (extract  [this ext-params]
    (ext-params 0)))

(deftype Genode
    [operation children]
  Extracted
    (extract [this ext-params]
      (if (empty? @children)   ;;terminating nodes have empty childen sequences
        (operation ext-params)
        (operation (concat (for [c @children] (extract c ext-params)))))
      )
    )

(defn applied  [f] (fn [args] (apply f args)))
(defn reduced [f] (fn [args] (reduce f args)))
(defn reversed [f] (fn [args] (f (reverse args))))

(defn conduce [f args]
  "takes a function and a sequence length n, returns a sequence of length n-1 whose entries are the combination of adjacent pairs, think pyrimid layering"
  (case (count args)
      0 args
      1 args
      (loop ;;if it has 2 arguments
       [smoke []
        spark (first args)
        flame (first (rest args))
        wood  (rest (rest args))]
       (if (empty? wood)
         (conj smoke (f spark flame))
         (recur (conj smoke (f spark flame))
                flame
                (first wood)
                (rest wood))
         )

       )
      )
  )

(defn conduced [f]
  "gives a conduced function which will conduce recursively with the given function until a single value is returned"
  (fn [args]
    (case (count args)
      0 0
      1 (first args)    ;; there is no way to appropriately default the single argument case
      2 (f (first args) (first (rest args)))
      (recur (conduce f args))
      )
    )
  )

(defn differ [& args]
  (let  [difference-square (for [i (range (count args)) j (range i)]
                             (math/abs (- (nth args i) (nth args j))))]
    (reduce + difference-square)
   )
  )

(def reduced-d (reduced differ))
(def combined-d (applied differ))
(def conduced-d (conduced differ))

(def mod 
  (fn [num div] 
    (if (= div 0)
     0 
    (clojure.core/mod num div))))

(def quot
    (fn [num div] 
    (if (= div 0)
     0 
    (clojure.core/quot num div))))

(comment
  (reduced-d [1 2 3])   ;stack collapse
  (combined-d [1 2 3])  ;web collect
  (conduced-d [1 2 3])) ;pyrimid apex

;;the leaf node is a genode made with no children
;;its operation is a dereferencing of the variable parameters passed to it as a vector or map
;;its parameter is the parameter by which the dereference occurs either index or key

(defn leaf-node
  ([param-number] (Genode. (fn [ext-params] (ext-params param-number)) (atom [])))
  ([] (leaf-node 0)))

(defn const-node
  ([const] (Genode. (fn [ext-params] const) (atom [])))
  ([] (const-node 0)))

(defn add-node
  ([default](Genode. (applied +) (atom [(const-node default) nil])))
  ([] (Genode. (applied +) (atom [nil nil]))))

(defn mod-node-r
  ([default]  (Genode. (reduced mod)
                         (atom [(const-node default) nil])))
  ([] (Genode. (reduced mod) (atom [nil nil]))))

(defn mod-node-l
  ([default](Genode. (reversed (reduced mod)) 
                      (atom [(const-node default) nil])))
  ([](Genode. (reversed (reduced mod)) (atom [nil nil]))))

(defn mult-node
  ([default] (Genode. (applied *) (atom [(const-node default) nil])))
  ([] (Genode. (applied *) (atom [nil nil]))))

(defn rquot-node
  ([default] (Genode. (reduced quot) (atom [(const-node default) nil])))
  ([] (Genode. (reduced quot) (atom [nil nil]))))

(defn cquot-node
  ([default] (Genode. (conduced quot) (atom [(const-node default) nil])))
  ([] (Genode. (conduced quot) (atom [nil nil]))))

(defn rdif-node
  ( [default] (Genode. (reduced differ) (atom [(const-node default) nil])))
  ( [] (Genode. (reduced differ) (atom [nil nil]))))

(defn cdif-node
  ([default] (Genode. (conduced differ) (atom [(const-node default) nil])))
  ([] (Genode. (conduced differ) (atom [nil nil]))))

(def basic-ops {:+ add-node
                :|> mod-node-r
                :<| mod-node-l
                :c  const-node
                :l  leaf-node
                :|-| cdif-node
                :* mult-node
                :q rquot-node})
