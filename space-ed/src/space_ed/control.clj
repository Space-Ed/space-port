(ns space-ed.control
  "a space for defining midi related control systems, the definition of event listeners that will then call core control functions
 a controller is the spark of the first event, this event may simply cause a change of some state with no fur

 user controls are set up by attaching a function like a poly player or a generator tree or a sequencer "
  (:require
   [overtone.core :refer :all]
   [space-ed.drivers :refer :all]
   [clojure.pprint :as pp]
   [clojure.set :as s]))

(comment
  (event-monitor-timer 5)
  (event-monitor-keys)
  (midi-connected-devices)
  (event-debug-off)
  (event-debug-on)
  (stop))

(defn default-midi [e]
  (pp/pprint (select-keys e [:timestamp :velocity-f :sel :note :velocity]))
  e)

(def ctl-keys [:on :off :ctl])

(def nil-patch {:on (fn [x] x)
                :off (fn [x] x)
               :ctl (fn [x] x)})


;;----------------------------CONTROLLER DEFINITION--------------------

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

(defn connected-names []
  (for [d (midi-connected-devices)] (d :name)))

;;create the pivots for all connected controllers
(def controllers (generate-control-source (connected-names) ctl-keys))


;; call connected names and alias them for convinience
(connected-names)

(def ctls
  {:midilink (controllers "Midilink")
   :kaos (controllers "KAOSSILATOR PRO")
   :axiom (controllers "Axiom A.I.R. Mini32")
   })


;;----------------------------------LAYERING-CONTROLS----------------------------------------------

(defn patch-control!
  "used to apply a patch onto control source, a patch will override the source function for the controller 
  therefore the sink must provide a function of an event map,
 it will only override the functions provided by the sink"
  [source sink]
  (doseq [ctk (s/intersection (set (keys source))(set(keys sink)))]
     (if (not (nil? (sink ctk)))
       (reset! (source ctk) (sink ctk))
       )
     )
  )

(defn apply-layer! [source layer ctl-type-key]
  "the king function, swaps the pivot with a function that passes the value through the old function 
  and then the layers function associated"
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
       (doseq [ctk (s/intersection (set(keys source)) (set (keys layer)))]
         (apply-layer! source layer ctk)
         ))))

(defn envelop! 
  ([source enveloping-func]
  "set all of the control pivots in the source to being the enveloping function, useful for clearing"
  (doseq [pivot (vals source)]
    (reset! pivot enveloping-func)))
  ([source enveloping-func inclusions]))

(defn caress!  [source caressing-func]
  "layer the caressing function on to all available pivots"
  (doseq [ctk (keys source)]
    (apply-layer! source {ctk caressing-func} ctk)))


;;------------------------Special Control Application--------------------------------------

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
    (patch-control! source dlayer)
    )
)

(defn distribution-fn [sinks ctl-type-key]
  (fn [e] 
    (doseq [sink sinks]
      (if (not (nil? (sink ctl-type-key)))
       (@(sink ctl-type-key) e)
        ))
    e))

(defn distribute! [source & sinks]
  "layer the source with a function that will call each of the sinks in turn for incoming events
   this allows for the same message to go to many drivers

   will only distribute if the source has the appropriate type"
  (let
    [source-keys  (keys source)
     distribution-layer (zipmap source-keys
                                (for [k source-keys](distribution-fn sinks k)))
    ]
    (layer-control! source distribution-layer)
    ))

(defn applied-distribute! [source number-splits]
  "distribute the source to a number of other splits.
  returns a sequence of splits to which the source will call with new events")

(defn apply-modal-control! [source key-sink-map]
  "Layer the source with a function that will call the sink mapped to by a mode key.
   also supports mode cycling for vector inputs(numeric keys) "
  )

(keys {:play "fuck" :work "fight"})
(range(count [3 4]))

(defn latch! [host parasite note-func-map ctk]
  "a splice helping function really it does a diversion but only to specific notes"
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
  "the parasite will only act if the note-func-map has an entry for the note in the event"
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
      [parasite (generate-control-source (keys source))]
    (feed! parasite  note-func-map ctk)
    (latch! source parasite note-func-map ctk)
    parasite))


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



(defn apply-split-patch! [source split-key split-controls & split]
 "source is a map of ctks to functions the source functions
   will be replaced with splitting functions that divert the call
   to the split controls 
   
eg.
   (def controls (generate-control-source [:a :b] [:on :off :ctl]))
   (applt-split-patch! (ctls :my-control) :note controls 0 :a 10 :b 40)

    will fire control pivot :a when  0 <= note < 10 and :b when 10 <= note < 40 
   "
 (let
   [type-keys (keys source)
    funcs     (for [k type-keys] (apply (partial split-fn split-key k split-controls) split))
    patch     (zipmap type-keys funcs)]
 (patch-control! source patch)
 )
)

;;----------------------Control Setup-----------------------
        
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
  ([set-control-fn]
      (doseq [pivot (vals controllers)]
        (envelop! pivot set-control-fn)))
  ([] (reset-controllers (fn [x] x))))


;;default controller setup
(reset-controllers  default-midi)  ;could be nil-patch
(remove-controllers)
(activate-controllers)

;;-------------------Specialized Controller Setup

;;here there is an issue because we basically want to have a 
(def rational-miditech {82 0 ,83 1, 28 2, 29 3, 16 4,  80 5, 18 6, 19 7, 74 8, 71 9, 81 10, 91 11, 2  12, 10 13, 5 14, 21 15})

(def rational-controller
  {:ctl (fn [e] (assoc e :note (rational-miditech (e :note))))})

(def neo-miditech (generate-control-source [:ctl]))
(divert! (ctls :midilink) neo-miditech rational-controller)

;;stop button stops
(def my-splice (splice (ctls :axiom) {16 (fn [x] (stop))} :ctl ))

;;-----------------Old Testing code------------------------

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

  (@((ctls :axiom) :on) {:note 48})
  
  (def split-a (split-fn :note 
                       :on 
                       {:a {:on (atom (fn [e] (println "i'm a!:" e)))}
                        :b {:on (atom (fn [e] (println "i'm b!:" e)))}}
                       10 :a 20 :b 30))

  (split-a {:note 31})
  
  (apply-split-patch! (ctls :axiom) 
                      :note 
                     {:a {:on (atom (fn [e] (println "i'm a!:" e)))}
                      :b {:on (atom (fn [e] (println "i'm b!:" e)))}}
                       10 :a 20 :b 30)
)


