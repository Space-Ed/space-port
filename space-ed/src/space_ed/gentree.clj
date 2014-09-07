(ns space-ed.gentree
  "this namespace is where to define generators that output selection indicies for instruments and sample sets"
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [space-ed.genode :as g]))

" a dynamic generator tree using the power of functional programming"

(defn vec-insert [vec-coll item insdex]
  (into (conj (subvec vec-coll 0 insdex) item) (subvec vec-coll (+ 1 insdex))))

(defprotocol Instree
  (extract [tree ext-params])
  (insert [tree node-key insertion-args])
  )

(defrecord Gentree [mode scope-dq root node-constructors]
  Instree
  (extract [tree ext-params]
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
  "create a generator tree
   Defaults:   
        mode: breadth first insertion
        operations: +, |-|, * , quot, mod-left, mod-right,
                     constant(c), bounce-input 
        root: nil"
  (Gentree. (atom :breadth-first) (atom []) (atom nil) g/basic-ops ))



;;before we get carried away with the possibilities of many many selection trees simultaneously
;;let us simply provide the access point for a single one

(def the-one (atom (gentree)))

(defn insert-one
  ([key](insert @the-one key []))
  ([key args] (insert @the-one key args)))

(defn extract-one [ext-params]
  (extract @the-one ext-params))

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
  "mapping note values to operation keys"
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

(defn g-control-single [note-key-map g-control-map]
  "get a controller for a single gentree
   there are 3 components 
    ops is the operation insertion uses on only
    nums is the  numerical constant insertion argument
         when a number is held the operations will be inserted with the argument
    cont is the controls for changing mode :toggle and resetting :reset

note-key-map is a mapping from note values to operation keys
g-control-map is a mapping from note values to :toggle or :reset"
  (let [g-tree (atom (gentree))
        held (atom nil)
        actions {:reset #(reset! g-tree (gentree))
                 :toggle #(toggle-mode! (. g-tree mode))}
        command-f (fn [{note :note on :velocity-f}]
                    (let [command (g-control-map :note)] 
                      (if (and (contains? actions command) (not= 0 on))
                        ((actions command)))))]
    {
     :ops {
           :on (fn [{note :note}]
                 (if (nil? @held)
                   (insert @g-tree (note-key-map note) [])
                   (insert @g-tree (note-key-map note) [@held])
                   ))}
     :nums {
            :on (fn [{note :note}]
                  (reset! held note))
            :off (fn [e]
                   (reset! held nil))}
     :cont {
            :ctl command-f
            :on command-f
            }
     :on (fn [e] (assoc e :note (extract @g-tree [(e :note)]))) 
     }
    )
  )
