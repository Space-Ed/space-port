(ns space-ed.contextualizers)

"This namespace is for development of musical contextualizers.

A contextualizer is a process taking a selection level input and mapping onto a
musical space like a scale or chord. 

The arcizer is an example of an contextualizer

others defined here
   manual contextualizer: Held keys will form a set to be selected from
   revolving contextualizer: a set of notes of given size is selected from
         and adding one will remove the oldest simultaneously, responds only 
         to :on commands that are not in the set

Any contextualizer of notes bears the problem of inversion, note spacing and position
we will use absolute interpretations initially"

(defn ordered-insert [coll item]

(defn manual-contextualizer []
  "get two control maps, one which takes the held notes"
  (let 
    [core (atom []) ;;the 
     ])
