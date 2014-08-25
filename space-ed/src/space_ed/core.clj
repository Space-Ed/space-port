(ns space-ed.core
  "the main namespace for the project, is a playground, testing station and performance loader which is to be the context that s given every possible power"
  (:use overtone.core)
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set]
            [space-ed.looping :as looper]
            [space-ed.samples :as samples]
            [space-ed.synths  :as syn]
            [space-ed.dividex :as dvx]
            [space-ed.control :as ctl]
            [space-ed.selection :as sel]
            [space-ed.arcizer :as arc])
)

(comment
  (stop)
  (boot-external-server)
  (kill-server)
  (midi-connected-devices)
  (samples/load-my-samples)
  (ctl/remove-controllers)
  (ctl/activate-controllers)
  (ctl/reset-controllers ctl/nil-patch))


(def period (atom 1000))
(reset! period 10000)
(def a-nice-pattern (dvx/merge-patterns (dvx/exclude-patterns (dvx/dividex-32 6)
                                                              (dvx/dividex-32 3))
                                        (dvx/dividex-32 5)))

;(ctl/set-control! :axiom sel/the-one-ctl)

(def atrol  (arc/arc-control))
(def s->arc (atrol :sub))
(def s->n   (ctl/sel->note 0 :on))
(def n->s   (ctl/note->sel 48 :on))
(def debug  (ctl/get-print-ctl))
(def axiom (ctl/ctls :axiom))
(def gentree sel/the-one-ctl)

(definst test-inst
  [freq 440]
  (out [0 1] (sin-osc:ar freq)))

;(def t (test-inst))

(def itrol (ctl/get-mono-player syn/the-generica [:freq 220]))


(comment
  (ctl/patch-control! axiom ctl/nil-patch)
  (ctl/layer-control! axiom n->s s->arc s->n itrol)
  (ctl/layer-control! axiom atrol)
  (ctl/layer-control! axiom (ctl/get-poly-player syn/the-generica)))


(comment

  (ctl/reset-controllers)

  (ctl/patch-control! axiom ctl/nil-patch)
  (ctl/apply-layer! axiom n->s :on)

  (ctl/apply-layer! axiom s->arc :on)
                                        ;(ctl/apply-layer! axiom s->n :on)
  (ctl/apply-layer! :axiom debug :on))

;(looper/start-dynamic-pattern period a-nice-pattern (fn [i] (sel/extract-one [i])) samples/onomataplayer)
