(ns setups.alabaster
  (:require [space-ed.control :as ctl]
            [space-ed.synths :as syn]
            [space-ed.arcizer :as arc]
            [recircules.core :as rec]))

(def input (ctl/ctls :axiom))

(def local-ctls (ctl/generate-control-source [:arc :drf :syn][:ctl]))

(ctl/apply-split-patch! input
                       local-ctls
                       1 :arc 5
                         :drf 9)

;;create looper
(def alabaser-loop rec/DRF-alpha)

(def loop-source (rec/loop-interpreter :DRF-loop ::alabloop))

