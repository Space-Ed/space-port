(ns recircules.fracticious
  (:require [overtone.core :refer :all]
            [recircules.core :refer :all]))

"This namespace is for the definition of fractional pattern generation systems.

These systems can be used in conjunction with density metrics and density targets
to generate patterns with well distributed features

Fractional systems are typically generated on the loop level. 
It is an iterative procedure of looking at each existing point and comparing 
its density to the integral of a target density function from the point to the next
if there is a deficit the point will introduce one more intervening the period to the 
next point. 

The process is repeated until adequte saturation level is attained 
or maximum iteration level is reached



It would be possible however to also do occurance level fracturing. 
This would involve scheduling forward to the next point but,
 if there is a deficit to schedule INSTEAD at <half>way between the present and the next point 

problematically this would result in getting off a rational split of the complete pattern

so the density would resolve to a target seperation.
 This seperation would be approached by binary search.

target 0.6666  0 -> 0.5 -> 0.75 -> 0.625 
 
Assumption: events can only occur at points in a pattern or at a hypthetical point 
partway between two adjacent points of the pattern or other hypthetical points,
provided that the hypthetical points are only available within the scope of
 the last event scheduling the 

The occurance level fractional system needs to carry some data forward that will orient 
itself within hierarchical time structure.



{:timestamp N
 :depth   N 
 :right [0,1]
 :cycle-pos N}

Evaluated
target-density (control reference)
actual-density (situation abstraction)

Decided
to be decidided is when to schedule the next events and what data they will contain.

Assumptions
-Only the left child is eligeble for scheduling(as it lies 
-apply-at/event-paradigm - no-action calls
-the left child may split the right n times
-arbitrary depth(runaway recursion danger)
-

The scheduled events would be passed forward the data which

A master call will kic off a cain of apply-at 

considering the data provided in the externalized events, this data will be
enumerate selection data for transformation and contextualization. 

The descision is based off an actual and target density comparison, each is normalised over
the points then a filter function is able to decide for each point. Density is calculated in this
case to be an increasing function of the recursion depth. The target in this case can 
simply be another value upon the interval of [0,1] so using sin^2 is an option this has an issue
that a value close to 1 will have runaway depth.  
"

(defn recursor [nome event-key r-data target-df actual-df]
  "the primary recursion carrier for fracticious activities"
  (let
    [{depth     :depth
      cycle-pos :cycle-pos
      cycle-index :cycle-index} r-data 
      beat (nome) ;;measuring the moment
      e-data {:note cycle-index
              :depth depth
              :position cycle-pos
              :density-f (actual-df depth)
             }
     ]
    
    (println "actual: " (actual-df depth) "\ntarget"(target-df cycle-pos)"\nposition:" cycle-index"\nbeat: " beat)
    
    (if (= depth 0)  ;;the top level carries forth the mantle in a steady fashion 
      (do (println "----spawning recursion---")
      (apply-at (nome (inc beat)) recursor nome event-key r-data target-df actual-df [])))
   
    (if (< (actual-df depth)(target-df cycle-pos))
      ;;then we recur here for left and right
      (let
        [left-data (assoc r-data :depth (inc depth))
         left-rec  (recursor nome event-key left-data target-df actual-df) ;immediate tunneling
         right-data (assoc r-data 
                           :depth (inc depth) 
                           ;;recur at a position another split beat forward
                           :cycle-pos (+ cycle-pos (/ 1 (clojure.math.numeric-tower/expt 2 (inc depth))))
                           :cycle-index (inc left-rec)) ;advance the cycle
         ]
        (println "scheduling forward to: " (right-data :cycle-pos) " the " (right-data :cycle-index)"th note in sequence")
        (apply-at (nome (+ beat (right-data :cycle-pos))) 
                  recursor nome event-key right-data target-df actual-df [])
         (right-data :cycle-index))
      (do (event event-key e-data)       ;only create an event when the recursion is to end
           cycle-index)                  ;return the current cycle index 
    )
   )
)
  
(defn launch [nome event-key target actual]
  (let [base {:depth 0
              :cycle-pos 0
              :cycle-index 0
              }]
   (recursor nome event-key base target actual)))

(defn my-triangle [x] 
  (+ 0.4 (if (< x 0.5)
           x
           (- 1 x))))

(defn hyperbolic-dd [d]
  (/ (+ d 0) (+ d 1)) 
  )

(defn exponential-dd  [d]
  (- 1  (clojure.math.numeric-tower/expt 2 (- d))))

(def nomus (metronome 6))

(launch nomus :recur my-triangle exponential-dd)

