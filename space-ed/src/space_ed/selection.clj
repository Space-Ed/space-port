(ns space-ed.selection
  "this namespace is where to define generators that output selection indicies for instruments and sample sets"
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [space-ed.genode :as g]))

" a dynamic generator tree using the power of functional programming"

(defn vec-insert [vec-coll item insdex]
  (into (conj (subvec vec-coll 0 insdex) item) (subvec vec-coll (+ 1 insdex))))

(defprotocol Instree
  (extract2 [tree ext-params])
  (insert [tree node-key insertion-args])
  )

(defrecord Gentree [mode scope-dq root node-constructors]
  Instree
  (extract2 [tree ext-params]
    (g/extract @root ext-params))
  (insert [tree node-key insertion-args]
    ;;first we check if there is anything on the stack, if not
    (let [node (apply (node-constructors node-key) insertion-args)]
      (if (= @root nil)
        (do (reset! root node) ;;this tree has had nothing installed so install
            (swap! scope-dq #(conj % node))
            true)
        ;;otherwise there is a root
        (if (empty? @scope-dq)
          false    ;;when there is nothing on the scope there can be no insertion
          (let [
                scope (peek @scope-dq)
                stack (pop @scope-dq)
                children @(. scope children)
                current-child (.indexOf children nil)  ;;the index of the first replicable character
                last-node? (= (- (count children) 1) current-child)  ;;the last element of children is the only nil, so this scope will not be readded
                ]
            (if (<= 0 current-child)                                  ;;if there is any open ends we insert the child
              (swap! (. scope children) #(vec-insert % node current-child)))

            (if last-node?
              (swap! scope-dq #(pop %)))                               ;;reinserting the current scope

            (case @mode
              :depth-first   (swap! scope-dq #(conj % node))           ;;inserting the new node ontop of stack(do next
              :breadth-first (swap! scope-dq #(vec (cons node %))))    ;;inserting new node bottom of stack(do last)
            true

            )
          )))))

(defn toggle-mode! [mode*]
  (if (= @mode* :depth-first)
    (reset! mode* :breadth-first)
    (reset! mode* :depth-first)
    )
  @mode*
  )

(defn gentree []
  (Gentree. (atom :breadth-first) (atom []) (atom nil) g/basic-ops ))

(comment
  (def t1 (gentree))
  (insert t1 :|-| [])
  (insert t1 :+ [])
  (insert t1 :* [])
  (insert t1 :<| [])
  (. t1 root)
  (extract2 t1 [5]))

;;before we get carried away with the possibilities of many many selection trees simultaneously
;;let us simply provide the access point for a single one

(def the-one (atom (gentree)))

(defn insert-one
  ([key](insert @the-one key []))
  ([key args] (insert @the-one key args)))

(defn extract-one [ext-params]
  (extract2 @the-one ext-params))

;we want to extend the extraction function to have the default argument to be
(defn call-count-appender [f counter*]
  "on a function with a sequence argument the function returned will first conjugate a call count before calling  on the sequence"
  (fn [args] (do
              (swap! counter* inc)
              (f (conj args @counter*))))
  )

(def extract-one-count (call-count-appender extract-one (atom 0)))

(defn reset-one []
  (def extract-one-count (call-count-appender extract-one (atom 0)))
  (reset! the-one (gentree)))

(def axiom-ctls
  {36 :+
   37 :|-|
   38 :*
   39 :q
   40 :c
   41 :l
   42 :<|
   43 :|>
   })

(def the-one-ctl
  (let
      [held (atom nil)]
      {:on (fn [{note :note}]
             (cond
              (< 38 note 44) (if (nil? @held)
                               (insert-one (axiom-ctls note))
                               (insert-one (axiom-ctls note) [@held])
                               )
              (< 66 note 79) (reset! held note)
              )
             )
       :off (fn [{note :note}] (reset! held nil))
       :ctl (fn [{note :note on :velocity-f}]
              (case note
                22 (reset-one)
                21 (toggle-mode!(. the-one mode))
                (println "no mapping here yet, note played: " note)
                )
              )
       :held held
       }))
