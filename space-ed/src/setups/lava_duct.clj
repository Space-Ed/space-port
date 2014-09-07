(ns setups.lava-duct
  (:use [space-ed.core]
        [space-ed.drivers]
        [space-ed.control]
        [space-ed.synths]
        [space-ed.gentree]))

(overtone.core/stop)
(reset-controllers)

"Attach a fractional generator to a gentree which is contextualized by a catepillar which is also the
driver of a  dreamy pad synth, and pass to a frequncy modulation lead"

(def axion (generate-control-source [:on :off]))
(divert! (ctls :axiom) axion {:on (fn [x] x)})

(def my-overlapse (overlapser 5 axion {1 :grow-f 2 :shrink-f}))

;;setup gentree
(def gcs (g-control-single axiom-ctls {0 :reset 1 :toggle}))
(def gentree-ops (generate-control-source [:on]))
(def gentree-nums (generate-control-source [:on :off]))
(def garr {:ops gentree-ops :nums gentree-nums :norm (generate-control-source [:on :off])})

(apply-split-patch! (ctls :axiom) :note garr 36 :ops 44 :norm 67 :nums 80)

(layer-control! gentree-ops (gcs :ops))

(layer-control! gentree-nums {:off (fn [e] (assoc e (- (e :note) 67)))
                              :on (fn [e](assoc e (- (e :note) 67)))}
                (gcs :nums))

;;setup fractional controller 




(def player (get-poly-player  cs80lead))


;((player :off) {:note 50 :velocity 50})

;(layer-control! axion my-overlapse player)
;(layer-control! (ctls :axiom) player)

(caress! axion default-midi)