(ns recircules.core
  ;(:use [overtone.core])
  (:require [clojure.math.numeric-tower :as math]
            [incanter.core :as inc]
            [overtone.core  :as ovt]
            [space-ed.samples :as samples]
            [space-ed.dividex :as dvx]
            
            ))
(samples/load-my-samples)

"This namespace is used to bring together the recircules system.
This system is a tool for developing intuitive rythmic density triggers
These triggers can be used in any case where a metronome can except they will be endowed with 
a note being that of the position in the loop and a velocity for the density value which the 
event was fired at.
"

(defn bound-expression [l u x]
  (let [top-margin    (- x u)
        bracket       (- u l)
        lower-margin  (- x l)
        q             (if (>= lower-margin 0)
                        (quot lower-margin bracket) 
                        (quot top-margin bracket)
                        )
        ]    
    (- x (* q bracket))))

(defn test-bounding []
  [(= 0 (bound-expression 0 1 1))
   (= 0.5 (bound-expression 0.25 0.75 0.5))
   (= 0.5 (bound-expression 0 1 1.5))
   (= 0 (bound-expression 0 1 1040))
   (= 0.5 (bound-expression 0.5 1 1.5))
   (= 0.75 ( bound-expression 0 1 -0.25))
   (= 0 ( bound-expression 0 1 -1))
   ]
 )

(defn circular-difference [l u a b]
  (math/abs (max  (bound-expression l u (math/abs(- a b)))  (bound-expression l u (math/abs(- b a))))))

(defn inverse-circular-difference [l u a b]
  (let [h (/ (- u l) 2)
        c (circular-difference l u a b)]
    (- (*(/(+ 1 h) h )
         (/ 1 (+ 1 c)))
       (/ 1 h))))

(defn cyclic-density [l u x others]
  (loop
    [densum 0
     to     (first others)
     rem    (rest others)]
    (let 
      [d (inverse-circular-difference l u x to)
       newsum (+ densum d)]
    (if (empty? rem)
      newsum
      (recur newsum (first rem) (rest rem))
      ))))

(defn densities [l u units-coll]
  "take the sum of absolute differences in a loop where 1 = 0"
  (loop 
    [mid (first units-coll)
     head []
     tail (rest units-coll)
     dees []]
    (let
      [d (cyclic-density l u mid (into head tail))
       newdees (conj dees d)]
      (if (empty? tail)
        newdees
        (recur (first tail)
              (conj head mid)
              (rest tail)
               newdees
          )))))

(defn norm-seq [coll] 
  (let [total (reduce + coll)]
    (for [d coll] (/ d total))))

(defn nu-densities [coll]
  "this inconpictuous function is the flagship for actual density calculation"
  (norm-seq (densities 0 1 coll)))

(nu-densities [0 0.1 0.2 0.6])

(def PI 3.1415926535897932384626433832795028841971693993751058)

(defn sinutrol [a b x y]
  "a cute density control function finding the density integral between two points a,b

x is the position of a density hump on the loop and 
y is its accentuation "
  (+ (bound-expression 0 1 (- b a)) (* (/ y (* 2 PI)) (- (inc/sin (*(+ x b)(* 2 PI))) (inc/sin (* (+ x a)(* 2 PI)))))))

(defn control-xy [f]
  (let [x (atom 0)
        y (atom 0)]
  {:x  x
   :y  y
   :f  (fn [a b] (f a b @x @y))}
  ))

(defn get-control-setter [target-map from-key to-key event-key]
  "used for creating wrappers of controls to map event types and keys onto arbitrary atom maps"
  {from-key (fn [e] (reset! (target-map to-key) (e event-key) ))}
)

(comment
(defmulti get-multi-control-setter [target-map crossing event-key]
  "a means of mapping a number of adjacent controls onto the ")

(defmethod get-multi-control-setter [crossing]) 
)

(defn target-densities [coll integrator]
  (loop [front (first coll)
         from (first coll)
         to    (second coll)
         rem   (drop 2 coll)
         result []]
    (if (empty? rem)
      (conj result (integrator from to) (integrator to front))
      (recur front to (first rem) (rest rem) (conj result (integrator from to)))
      )
    )
  )

 
 ;; The sin control works for singe points of application, arbitrary number of points will require some notion of dft
 ;; because the overtone fft is behind the scenes it wont be of use besides it would be useful to implement a regular dft 
 ;; considering the number of points will be low, within comprehensible range( < 7 )
 
 ;; the first mechanism is a function of a rhythm sequence for each point in the sequence we will create a value in {0,1}
 ;; the looping mechanism will then interpret this rhythm package 
 ;; to spice it up a dependence upon random value will be introduced. 
 ;; completely random would be 50-50 chance of expression regardless of density.
 ;; completely determined would be decided by whether the target density lower than the actual.
 ;; in balanced rhythms we would have homognenious density so a clustering effect would ensue.
 ;; to get the balance the difference would be given to a function with the chaos parameter 
 ;;the random value would need to exceed a certain density.
 ;; to multiply by the unit parameter would be enough 
 
 ;; with 5 points say uniform would give each a value of 0.2 the target could be anywhere on [0,1]
 
 ;; there is this notion of tolerance. high tolerance lets many through low tolerance lets none through.
 ;; this is a matter of reducing the 
 (defn decide-random [target actual stochastic tolerance]
   "a function for deciding whether to trigger the too sparse flag (1) based off the target
a and actual densities. 

Stochastic [0,1] default:0  gives the degree to which the system may randomly defy the tolerance boundary 
0 means no randomness 1 means ignore the boundary all together

tolerance [0,1] default:0.5 gives the base threshold that will allow 1 to pass 
0 means nothing can pass 0.5 means positive can pass 1 means everything passes "
   (let 
     [diff (- actual target)    ; negative is a deficiency
      t    (- (* 2 tolerance) 1) ; t on [-1, 1]
      r-upper (+ t (* stochastic (- 1 t))) ;r on [t + s(-1 - t),t + s(1-t)]
      r-lower (+ t (* stochastic (- -1 t)))
      r-val (+ r-lower (* (rand) (- r-upper r-lower )))
      ]
     (if (< r-val diff)
       0  ;fail to break 
       1)
     )
 )
 
 (defn co-filter [f filter-seq & other-seqs]
   "filters the other seqs by the truth of the filter function.

The other seqs are supposed to be the same length the function will stop if any seq runs short"
   (let 
     [num-seqs   (count other-seqs)
      min-length (min (count filter-seq) (if (> num-seqs 0)
                                           (apply min (for [s other-seqs] (count s)))
                                           (count (first other-seqs))))]
   (loop 
     [index 0
      result (for [s (range num-seqs)][])]
     (if (>= index min-length)
        result 
        (recur (inc index)
               (if (f (nth filter-seq index))
                   (for [i (range num-seqs)] (conj (nth result i) (nth (nth other-seqs i) index)))
                   result
                   )
               )
        )
     )
   )
 )

 (defn st-filter-func []
   (let
     [s (atom 0)
      t (atom 0.5)
      f (fn [target actual](decide-random target actual @s @t))]
     {:s s
      :t t
      :f f}
     )
 )
 
 ;;density regulation filter gives the {0,1} values after the density is calculated and the difference is 
 (defn density-regulation-filter [points actual-df target-df filter-f]
   (let
     [density-seq (actual-df points)
      target-seq (target-densities points target-df)
      filter-seq (for [i (range (count points))] (filter-f (nth  target-seq i)(nth  density-seq i)))
     ]
     (co-filter #(= 1 %) filter-seq points)
     )
   )
 
 
(def sin-ext-ctl (control-xy sinutrol)) ;target density controller(integral)
(def filter-f-alpha (st-filter-func))   ;filtering function


(defn DRF-loop 
  ([period drf points-source loop-key]
    (let [time (ovt/now)
          filtered-points (first(drf @points-source))
          c (count filtered-points)]
      (doseq [i (range c)]
        (ovt/apply-at (+ time (* (nth filtered-points i ) period)) ovt/event loop-key {:note i} [])
        )
      (ovt/apply-at (+ time period) DRF-loop period drf points-source [])
      )
    )
  ([period drf points-source] (DRF-loop period drf points-source :DRF-loop))
  )

(defn loop-interpreter [loop-key handler-key]
  "creates an atomic source that will be called by a handler declared here with the handler key
   
   picks up events 

   useful for loops to attach many interpreters to the same looping pattern
   this is a special case where :loop control types are interpreted. This differentiates it from on off ctl"
  (let 
    [source (space-ed.control/generate-control-source [:loop])]
     (ovt/on-event loop-key 
                   (fn [e] (@(source :loop) e))
                   handler-key)
     (assoc source :handler handler-key)))

(comment
  "A TEST PATTERN"
   (ovt/event-debug-on)
  
  (def DRF-alpha #(density-regulation-filter % 
                                           nu-densities 
                                           (sin-ext-ctl :f) 
                                           (filter-f-alpha :f)))
     
 
 (reset! (sin-ext-ctl :x) 0.1)
 (reset! (sin-ext-ctl :y) 1)
 
  (DRF-alpha [0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])

  (def pattern (atom (dvx/dividex-32 13)))
  
  (ovt/event-debug-off)
  (first(DRF-alpha @pattern))
  (DRF-loop 5000 DRF-alpha pattern)
  (def l1 (loop-interpreter :DRF-loop ::loop1))
  (space-ed.control/apply-layer! l1 {:loop (fn [e] (println e))} :loop)
  ((l1 :loop) {:note 30})
  (ovt/remove-event-handler ::loop1)
)





   
     
