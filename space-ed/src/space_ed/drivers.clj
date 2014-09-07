(ns space-ed.drivers
  (:require [overtone.core :refer :all]
            ))

;------------------------------INSTRUMENT CONTROLLERS------------------------

(defn memoize-on-off
  "the function will wrap up an on controller and give it off functionality by maintaining a dynamic map of the inputs to outputs, this way the inputs will always correspond across so that the off applies to the right entity even for stateful controllers

  a few assumptions must be clear about
0: NO Side effects
1:  any single input value should be turned on only once before being turned off again and turned off only once
2: there is the possibility of the same output value being turned on twice, it will then be turned off twice also"
  [untinkered]
  (let
      [cross-map (atom {})
       old-on    (untinkered :on)
       new-on    (fn [x]  ;memo by note
                   (let
                       [in-val (x :note)
                        out-val  (old-on in-val)]
                     (swap! cross-map assoc in-val out-val)
                     (assoc x :note out-val )))
       amended-control (assoc untinkered :on new-on)

       off-control (fn [x] (assoc x :note (@cross-map (x :note))))
       ]
    (assoc amended-control :off off-control :cross-map* cross-map
           )))

(defn adjustable-offset-ctl
  "an on function which simply offsets the note by the addition of a numeric control"
  [initial]
  (let
      [adjust (atom initial)]
    {:on (fn [x] (+ x @adjust))
     :adjust* adjust}))

(defn offset-wrapper
  "simply fiddes with the :on function to introduce an offset at the input and output for every event passed to the control function"
  [wrapup input-shift output-shift]
  (let
      [old-on (wrapup :on)]
    (assoc wrapup :on (fn [e]
                        (assoc e :note (+ output-shift
                                               (old-on (+ input-shift
                                                          (e :note)))))))))


(defn get-poly-player
  "returns a basic midi event interpreter. reading on and off messages

   It is possible to provide a mapping to guide the control of the synths so that the gate in {0, 1}
   can be used to switch and number of gates on the instrument and vel in[0,1] may control any fixed range parameter
 
   Synthy: a synthdef that will be played. must have the key :freq

Gate-map: a map of synth parameter keys to functions that interpret the gate. useful for controlling several parameters with the gate.
  default: {:gate pass}
vel-map: a map of synth parameter keys to interpreting functions 
  default: {:vel pass}

eg 

;;create synth
(defsynth my-synth [freq 400 vel 1 gate 1] (hot-stuff freq vel gate))

;;assign poly player
(def pp (get-poly-player my-synth))

;; play a note
((pp :on) {:note 40 :velocity 0.678})
"
  ([synthy gate-map vel-map]
      (let
          [notes* (atom {})
           gate-setter (fn [g] (fn [e] ;;anonymous function is the gate setter with g = 1 or 0 for on or off sets other parameters too
                                (let [note (:note e)
                                      freq (midi->hz note)
                                      kvel (if (nil? (:velocity e)) 1 (:velocity e))    ;; if not given set to 1
                                      vel  (/ kvel 127)
                                      inst-params  (concat
                                                    [:freq freq]
                                                    (interleave
                                                      (keys gate-map) (for [v  (vals gate-map)] (v g) ))
                                                    (interleave
                                                      (keys vel-map)  (for [v  (vals vel-map)] (v vel) )))
                                      ]
                                  (println inst-params)

                                  (if (node-live? (@notes* note))
                                    (apply ctl (cons (@notes* note) inst-params))                  ;;set instrument parameters
                                    (swap! notes* #(assoc % note (apply synthy inst-params))))     ;;replace dead/non-existent synth
                                  )))]
        {:on (gate-setter 1)
         :off (gate-setter 0)
         :notes* notes*}))
 ([synthy]
    (get-poly-player synthy {:gate (fn [g] g)} {:vel (fn [g] g)}))
 )


(defn get-mono-player
  "A mono player is simply charged with adjusting the frequency of a synth and creating an instance in the first case,

   there is an assumption that the synth is never off or its other characteristics are controlled from elsewhere,
"
  ([synthdef init-args ctl-key ctl-f]
      (let
          [synthstance (apply synthdef init-args) ]
        {:on (fn [{note :note}] (ctl synthstance ctl-key (ctl-f note)))
         :synth synthstance}))

  ([synthdef init-args] (get-mono-player synthdef init-args :freq midi->hz))
  )

(defn get-mono-replayer
  "the same as above but will keep instantiating new synths and setting the gate key to off for the old,

   there may be an issue with off, ideally when all are off the "
   ([synthdef init-args ctl-key ctl-f gate-key]
      (let
          [synthstance (atom nil) ]
        {:on (fn [{note :note}]
               (if (node-live? @synthstance)
                 (ctl @synthstance gate-key 0)) ;;turn off live synths
               (reset! synthstance (apply synthdef ctl-key (ctl-f note) gate-key 1 init-args)))
         :synth* synthstance}))

  ([synthdef init-args] (get-mono-replayer synthdef init-args :freq midi->hz :gate)))

(defprotocol RollingQueue 
  (push [this e])
  (grow [this])
  (shrink [this]))

(defn get-catipillar [n]
  (let [contents (atom [])
        size     (atom n)
        pusher (fn [e]             
               (cond
                 (< (count @contents) @size) (do (swap! contents #(vec (conj % e))) ;push
                                                      nil)
                 (== (count @contents) @size) (let
                                               [front (first @contents)]
                                               (println "shuffle")
                                                (reset! contents (vec (rest (conj @contents e))));push
                                                       front)
                 (> (count @contents) @size) (let
                                               [front (first @contents)]                                                
                                               (swap! contents #(vec (rest %)))  ;pop
                                                      front)              
                 :default "garbage")
               )
        contextualizer (fn [e] 
                         (assoc e :note (nth @contents (mod (e :note) (count @contents)))))]
    {:push-f pusher
     :grow-f (fn [] (swap! size inc))
     :shrink-f (fn [] (swap! size dec))
     :contents contents
     :ctx-f contextualizer}))

(def ike (get-catipillar 4))
((ike :push-f)3)
((ike :shrink-f))
@(ike :contents)
((ike :ctx-f) {:note -2})
        

(defn overlapser [n off-source ctl-map]
  "get an overlapse controller that will interpret on messages only
 but allow only a finite number to come through
   once there are n on the next on will trigger an off for the first in

   the off source needs to be provided, so that it can be called when the crawl occurs
   a controller with an existing off attached to event handlers should be removed  

   thus we assume that no other off funciton will be present and will add one 
 "
  (let 
    [catepillar (get-catipillar n)]
    (reset! (off-source :off) (fn [e] e))
    {:on (fn [e]
           (let [popped ((catepillar :push-f) (e :note))]
             (if (not (nil? popped))
                      (@(off-source :off) (assoc e :note popped))
             )
             e
             )
           )
     
     :off (fn [e] e)
     
     :buttons (fn [e]  ;;assign buttons to a split and map thier note values to the buttons
                (let [button-key (ctl-map (e :note))]
                  (if (not (nil? button-key))
                    ((catepillar button-key)))))
     :catepillar catepillar
     })
  )




                      
             
  
  
  
  