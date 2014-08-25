(ns space-ed.control
  "a space for defining midi related control systems, the definition of event listeners that will then call core control functions
 a controller is the spark of the first event, this event may simply cause a change of some state with no fur

 user controls are set up by attaching a function like a poly player or a generator tree or a sequencer "
  (:require
   [overtone.core :refer :all]
   [clojure.pprint :as pp]))

(comment
  (event-monitor-timer 5)
  (event-monitor-keys)
  (midi-connected-devices)
  (event-debug-off)
  (stop))

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


(defn default-midi [e]
  (pp/pprint (select-keys e [:note :velocity :data1 :data2 :timestamp :velocity-f :sel]))
  e)

(def controllers
  {
   "Midilink"
   {
    :on  (atom default-midi)
    :off (atom default-midi)
    :ctl (atom default-midi)
    }
   "PRO [hw:3,0,0]"
   {
    :on  (atom default-midi)
    :off (atom default-midi)
    :ctl (atom default-midi)
    }
   "Axiom A.I.R. Mini32"
   {
    :on  (atom default-midi)
    :off (atom default-midi)
    :ctl (atom default-midi)}
   }
  )
(def ctl-keys [:on :off :ctl])

(def ctls
  {:midilink (controllers "Midilink")
   :kaos (controllers "PRO [hw:3,0,0]")
   :axiom (controllers "Axiom A.I.R. Mini32")
   })

(def nil-patch {:on (fn [x] x)
                :off (fn [x] x)
               :ctl (fn [x] x)})
;;the patching could involve creating a split so that multiple assignments made or require the split be given as the source
(defn patch-control!
  "used to apply a patch onto control source, a patch will override the source function for the controller 
  therefore the sink must provide a function of an event map,
 it will only override the functions provided by the sink"
  [source sink]
  (doseq [ctk ctl-keys]
     (if (not (nil? (sink ctk)))
       (reset! (source ctk) (sink ctk))
       )
     )
  )

(defn memoize-on-off
  "the function will wrap up an on controller and give it off functionality by maintaining a dynamic map of the inputs to outputs, this way the inputs will always correspond across so that the off applies to the right entity even for stateful controllers

  a few assumptions must be clear about

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

(defn apply-layer! [source layer ctl-type-key]
  "the king function, "
  (if  (not (nil? (layer ctl-type-key)))  ;we are applying a new function
    (swap! (source ctl-type-key) (fn [oldfn] (fn [x]  ((layer ctl-type-key) (oldfn x)))))
    )
  )

(defn layer-control!
  "used to apply successive processes to a command map,
 so replaces the functions :on, :off, :ctl with functions that pass the result through the functions provided, thus each successive argument must allign, reccomend use of maps and check for nil key access in every function

  if the layer does not provide a function for a key the source function is kept"
  ([source & layers]
     (doseq [layer layers]
       (doseq [ctk ctl-keys]
         (apply-layer! source layer ctk)
         ))))

(defn get-print-ctl []
   (let
       [ctl-funcs  (for [k ctl-keys] default-midi)
       ctl-map    (apply hash-map (interleave ctl-keys ctl-funcs))]
    ctl-map
    ))

(defn sel->note [offset key]
  {key (fn [x] {:note (+ offset (x :sel))})})
(defn note->sel [offset key]
  {key (fn [x]  (- (x :note) offset))}
  )

(defn get-cross-map 
  "bridge the differences with a spin"
  ([e-type key-from key-to f]
  {e-type (fn [e] (assoc e key-to (f (e key-from))))})
  ([e-type key-from key-to] (get-cross-map e-type key-from key-to (fn [e] e)))
  )
;;------------------------META CONTROLS--------------------------------------

(defn modal-control-fn[m-key-control-map]
  "sometimes we want a controller to have access to many modes of operation, the controller itself provides the commands for switching between modes, three mode switching methods  are cycling, direct and shifting, cycling is appropriate when ease of addition is important, direct is when we dedicate a range of numbers to picking particular modes, shifting is having a shift key to access modes by direct selection ,

The biggest issue is that one wants external controls to be the meta keys, then we would need to provide access to a "
  (let
      [mode (atom (first (keys m-key-control-map)))
       ctl-funcs  (for [k ctl-keys] ((m-key-control-map @mode) k))
       ctl-map   (apply hash-map (interleave ctl-keys      ))]
    (assoc ctl-map :mode mode)
    ))


(defn new-controller []
  (let [keys ctl-keys
        vals (for [k ctl-keys] (atom (fn [pass] pass)))]
    (apply hash-map (interleave keys vals))))

(defn latch! [host parasite note-func-map ctk]
  (patch-control!
   host
   {ctk (fn [e]
          (let [unspliced? (nil? note-func-map)
                legacy      @(host ctk)]
            (if unspliced?
              (legacy e)
              (@(parasite ctk) e))))})
  )

(defn feed! [parasite note-func-map ctk]
  (patch-control!
   parasite
   {ctk (fn [e]
          (let [func (note-func-map (e :note))]
            (if (not (nil? func))
              (func e))))}))

(defn splice
  "take designated notes away from one controller and feed to another that is created to call the functions belonging to those notes "
  [source note-func-map ctk]
  (let
      [parasite (new-controller)]
    (feed! parasite  note-func-map ctk)
    (latch! source parasite note-func-map ctk)
    parasite))

(def my-splice (splice (ctls :axiom) {16 (fn [x] (stop))} :ctl ))

(defn generate-control-source
   ([ctl-keys types] (loop
                       [key (first ctl-keys)
                        rem-keys (rest ctl-keys)
                        split-map {}]
                       (let [next-type-map
                                 (loop [type (first types)
                                        type-rem (rest types)
                                        type-map {}]
                                   (let [next-val (atom (fn [x] x))
                                         next-map (assoc type-map type next-val)]
                                     (if (empty? type-rem)
                                       next-map
                                       (recur (first type-rem)
                                              (rest type-rem)
                                              next-map
                                             ))))
                              next-ctl-map (assoc split-map key next-type-map)]
                         (if (empty? rem-keys)
                           next-ctl-map
                           (recur (first rem-keys)
                                  (rest rem-keys)
                                  next-ctl-map)
                           )
                         )
                       ))
  ([types] ((generate-control-source [:_] types):_) )
 )


(defn split-fn [split-key type-key split-controls & split]
  "take from the split in sets of 3 seperated by gaps of 2"
  (fn [e]
      (loop
        [[l k u] (take 3 split)
          rem    (drop 2 split)]
        (let [
              val (e split-key)
              key-out (if (<= l val (dec u))
                        k 
                        nil)]
          (if (not (nil? key-out)) ;;then we have a winner call the appropriate func
            (@((split-controls key-out) type-key) e)
             ;;otherwise 
             (if (= 1 (count rem));drop has resulted in the last edge remaining no key found, ignore
               nil
               (recur 
                 (take 3 rem)
                 (drop 2 rem))))))))

(def split-a (split-fn :note 
                       :on 
                       {:a {:on (atom (fn [e] (println "i'm a!:" e)))}
                        :b {:on (atom (fn [e] (println "i'm b!:" e)))}}
                       10 :a 20 :b 30))

(split-a {:note 31})

(defn apply-split-patch! [source split-key split-controls & split]
 "source is a map of ctks to functions the source functions
   will be replaced with splitting functions that divert the call
   to the split controls 
 
   (applt-split {:on (atom(fn [x] x))} {:a (atom (fn)) :b (atom (fn))} 0 :a 10 :b 40)
      
  {:on (atom (fn [e] (cond (<= 0 (e split-key) 9) (@((split-controls :a):on) (e split-key))
                           (<= 10 (e split-key) 40)(@((split-controls :b):on) (e split-key)))))}
   "
 (let
   [type-keys (keys source)
    funcs     (for [k type-keys] (apply (partial split-fn split-key k split-controls) split))
    patch     (zipmap type-keys funcs)]
 (patch-control! source patch)
 )
)

(apply-split-patch! (ctls :axiom) 
                    :note 
                     {:a {:on (atom (fn [e] (println "i'm a!:" e)))}
                        :b {:on (atom (fn [e] (println "i'm b!:" e)))}}
                       10 :a 20 :b 30)
        
;;(@((ctls :axiom) :on) {:note 20})

  
(defn key-selection [bottom-end select-func]
  {:on (fn [e] )})

(defn activate-controllers
  "sets up the listeners for all the midi controllers, new midi controllers need to be setup like the existing"
  []

  (on-event [:midi :note-on]
            (fn [e] (@((controllers ((e :device) :name)) :on) e))
            ::on-event-handler)

  (on-event [:midi :note-off]
            (fn [e] (@((controllers ((e :device) :name)) :off) e))
            ::off-event-handler)

  (on-latest-event [:midi :control-change]
            (fn [e] (@((controllers ((e :device) :name)) :ctl) e))
            ::ctl-event-handler)
  )

(defn remove-controllers
  "gets rid of the controllers so that new listeners can be made"
  []
  (remove-event-handler ::on-event-handler)
  (remove-event-handler ::off-event-handler)
  (remove-event-handler ::ctl-event-handler)
  )


(defn reset-controllers
  ( [set-control]
      (doseq [k (vals ctls)]
        (patch-control! k set-control)))
  ([] (reset-controllers nil-patch)))


;;default controller setup
(reset-controllers (get-print-ctl))  ;could be nil-patch
(remove-controllers)
(activate-controllers)

;;-------------------Specialized Controller Setup

;;here there is an issue because we basically want to have a 
(def rational-midilink {82 0 ,83 1, 28 2, 29 3, 16 4,  80 5, 18 6, 19 7, 74 8, 71 9, 81 10, 91 11, 2  12, 10 13, 5 14, 21 15})

(def rational-controller
  {:ctl (fn [e] (assoc e :note (rational-midilink (e :note))))})

(defn diversion-fn [func sink key]
  "creates a layer that instead of just calling the function will call the sinks function with the value"
  (fn [e] (@(sink key) (func e))))

(defn diversion-layer [layer sink]
  (let 
    [dkeys (keys layer)
     diverted-funcs (map #(diversion-fn (layer %) sink %) dkeys)]
    (zipmap dkeys diverted-funcs)))
    

(defn divert! [source new-source intermediary-layer]
  "divert calls to the source onto the new source thus allowing us 
to patch the new source without stomping the old source 

diversion will only apply as the intermediary-layer applies
 "  
  (let 
    [dlayer (diversion-layer intermediary-layer new-source)]
    (layer-control! source dlayer)
    )
)

(layer-control! (ctls :midilink) (get-print-ctl))

(def rational-miditech (generate-control-source [:ctl]))

(reset! (rational-miditech :ctl) default-midi)

(@(rational-miditech :ctl) {:note 82})

(def divert (diversion-layer rational-controller rational-miditech))
((divert :ctl) )

;;(layer-control! (ctls :midilink) divert)
;;(divert! (ctls :miditech) rational-miditech rational-controller)


(comment
  
  (patch-control! (ctls :axiom) nil-patch)

  (def n->s (note->sel 48 :on))
  (def s->n (sel->note 0 :on))

  ((n->s :on) {:note 48})
  ((s->n :on) {:sel 2})
  ((n->s :on) (@((ctls :axiom) :on) {:note 48}))

  (reset-controllers)
  (swap! ((ctls :axiom) :on) (fn [oldfn] (fn [x] ((n->s :on) (oldfn x) ))) )

  (apply-layer! (ctls :axiom) n->s :on)
  (apply-layer! (ctls :axiom) s->n :on)
  (apply-layer! (ctls :axiom) (get-print-ctl) :on)

  (@((ctls :axiom) :on) {:note 48}))


