(ns setups.alabaster
  (:require [space-ed.control :as ctl]
            [space-ed.synths :as syn]
            [space-ed.arcizer :as arc]
            [recircules.core :as rec]
            [recircules.fracticious :as frac]
            [space-ed.dividex :as dvx]
            [space-ed.transform :as trans]))

(overtone.core/midi-connected-devices)
(overtone.core/stop)
(overtone.core/event-debug-off)

;;primary control source
(def input (ctl/ctls :axiom))

;;control pivots
(def local-ctls (ctl/generate-control-source [:arc :drf :syn][:ctl]))

(ctl/apply-split-patch! input
                        :note
                       local-ctls
                       1 :arc 5
                         :drf 9)

;;create and tap looper
(def base-pattern (atom dvx/dvx-3+5))
(reset! base-pattern (dvx/dividex-32 18))
(def loop-ctl (rec/DRF-control {5 :s 6 :t 7 :p 8 :e}))
(def abloop (rec/DRF-loop 2500 (loop-ctl :drf) base-pattern :abloop1))
(def loop-source (rec/loop-interpreter :abloop1 ::alabloop))

(ctl/layer-control! (local-ctls :drf) loop-ctl) 
;

;;create Arcizer
(def a1 (arc/arc-control))

(ctl/layer-control! (local-ctls :arc) a1)
;;create instrument controller
(def fm-player (ctl/get-mono-player syn/fm-gogo [] :base-f overtone.core/midi->hz))

;;link up primary chain
(ctl/layer-control! loop-source (a1 :sub) fm-player)

;;pizazz
(ctl/layer-control! input (ctl/get-poly-player syn/cs80lead)) 

;;fun-fun
(overtone.core/ctl (fm-player :synth) :mod1-dr 1 :mod1-fr 2)

