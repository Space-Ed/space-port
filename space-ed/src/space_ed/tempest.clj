(ns space-ed.tempest
  (:use overtone.core)
  (:require [space-ed.control :as ctl]
            [space-ed.arcizer :as arc]
            [space-ed.synths  :as syn]
            [space-ed.selection :as gen]
            [space-ed.samples :as smp]
            [space-ed.dividex :as dvx]
            [recircules.core :as rcs] ))

;;module for harmonic transformations
(def arcizer (arc/arc-control))

(defn afunc [x](((arcizer :sub) :on) x))

(defn loopplay [e] ((smp/onomatapaea (e :note))))

(def litconc [[0][0 1][0 2]])

(defn litconcer [litconc]
  (fn [e] (litconc (mod (e :note)(count litconc)))))

(def ah (litconcer litconc))
    
(ah {:note 23})

;;setup DRF-loop
(def pattern (atom (dvx/dividex-32 13)))
(DRF-loop 5000 DRF-alpha pattern)

;;DRF-loop interpretation
(on-event :DRF-loop 
          loopplay
          ::seq-listen1)

(def player (ctl/get-poly-player syn/b3 {:gate (fn [x] x)} {:a (fn [x] (/ 1 (+ 1  x)))} ))
(ctl/layer-control! (ctl/ctls :axiom) player)
(ctl/reset-controllers)
(stop)

(demo (out [0 1] (in:ar 0)))