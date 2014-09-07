(ns recircules.induction
  (use [space-ed.core]
       [recircules.core]))

"this namespace is for the creation of systems that interpret midi events and introduce them into loops

There is a foundational structure to consider here, it represents a loop sequencing. 

The appropriate structure for this is an ordered vector of proportions, a pattern and 
another vector of corresponding entities present at the time that proportion encodes.

Once a structure like this is attained one can create an event trigger interpretation that schedules from the structure once per revolution.
this however puts the modification granulatity on the scheduling level, an advancement it will be when we put scheduling on the occurance level. 

Scheduling levels:
   Absolute Level : total automation
   Loop level  : events are scheduled at the beginnning and respond to changes in the structure made in the previous phrase.
   point Level : events are scheduled at a finite number of positions within the loop. so events can be decided some time before the point they will appear
                 but which positions present can only be changed on the loop level
   
   OccuranceLevel :events are scheduled at the occurance of each event.
                  This means we have the power to change what will occur as effect before the cause occurs. 
                  The immediate application is to event delays.
   
   pollingLevel : an extreme of the point level where there is the illusion of continuous change

Quantized Delays:
Using an Occurance level scheduling seems to eliminate the need for a loop phrase altogether but we want to keep this around to make it syncable. 
Instead of binning it completetly we can access it in order to find our position(our last position) or an advanced position in the loop. 
We then can shcedule to an advanced position.  

Density regulation 
   Loop level : recircules.core explores this events of the complete structure are only sometimes represented as interpretable events  
   Point Level: at each point decide whether the event should appear here.
   Occurance Level: before scheduling the next make sure it won't be too dense

Quantized induction:
  entities are induced onto points and points are induced onto loops.
fractional induction is also a potentially mind breaking idea. It would involve a binary zoom into the point of the played entity. 
Entities created on a highly responsive precise level would have delays that approximate real time. 
On the other hand entities on a highly quantized level will have quantized delays.

This would allow us to smoothly transition between a normal delay for lead voices and a quantized delay for beats.

there is potentially an issue with off kilter patterns, do we use binary fractions or more directly aim for 

Looping patterns(permutable):

key literal 
A->A   delay
A->B->A  tic-tac delay
A->B->C->A

A -> P(A) where P is a full length permutation of n elements

Functional delay:
events recieved will trigger events of the same type but transformed by changing the values such as velocity and note.

Stage 1:
A point inducting (1 key) control and a delay quantized to the points. Both on and off events are to be occurance scheduled .
fixed period. 


Stage 2: 
A point inducting control with scheduling for on and off coupled with a deduction mechanism
 to eliminate based off the pattern ON(B) ON(A) OFF(B) -> Del(A)

Stage 3:
A scheme for multiple sequencers for many independent rhythms 

Stage 4:
Density Regulated filtration


"

(defn induct! [pattern* point]
  "introduce this new point into this one in the appropriate location,
   works generically as in order insertion"
  (let 
    [new-pattern    (loop [head []
                           mid (first @pattern*)
                           tail (rest @pattern*)]
                      (if (empty? @pattern*) 
                        [point]              ;; empty pattern case. 
                        (if (nil? mid)
                          (conj head point)    ;;the end case
                          (if (< point mid)     ;; mid is not nil the first
                             (into (conj head point mid) tail)
                             (recur (conj head mid)
                                    (first tail)
                                    (rest tail))
                           ))))]
    (reset! pattern* new-pattern)))

(defn position-in-loop [point points]
  "return the position of the point in the loop assuming that the beginning of the "
  )

(defn make-inductor []
  "produces an interpretive layer that passes the event through but puts a new "
 {:on (fn [e] 
        (let [t (e :timestamp)]
          (if (not (nil? t)))))
  }
 )
            

(defn ceiling-quantize [in-coll value]
  "returns the next value in in-coll higher than or equal to value"
  (loop [cap []
         front (first in-coll)
         buttress (rest in-coll)]
    (if (< value front)

(defn floor-quantize [in-coll value]
  "returns the value of in-coll equal to or immediately beneath value")

(defn pattern-quantize-c [pattern quantize])
(defn pattern-quantize-f [pattern quantize])

(defn quantized-pos [quantize value]
  "quantization index")

(defn quantized-inc [quantize value i]
  "returns the value at an index i along from the index of the quantized value,
   if the increment exceeds the bound it will continue with an addition of 1, 
   this is assuming that the quantization vectors values are on [0,1] ")




