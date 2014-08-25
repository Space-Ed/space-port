(ns space-ed.arcizer
  (:require [overtone.core :as o]
            [overtone.music.pitch :as p]
            [space-ed.control :as ctrl]))


"this file captures arc theory, which will take a section of the circle of fifths
 and use it to generate sequences that will quantize integer input data to produce harmonic appregios"

 ; the process
 ;position + width -> #{tones}
 ;#{tones} + bass notes -sequencing-> [pitches]
 ;+ octave-sel ->


(def circle-of-fifths [:C :G :D :A :E :B :F# :C# :G# :D# :A# :F])
(def circle-of-halves [:C :C# :D :D# :E :F :F# :G :G# :A :A# :B])
(p/NOTES :d)

(defn pos->note [pos]
  (circle-of-fifths (int (* pos 11))))

(defn circle-arc [pos width]
  (let [cyc (cycle circle-of-fifths)]
    (for [i (range width)]
      (nth cyc (+ i pos)))))


(defn get-mod [pos coll]
  (nth coll (mod pos (count coll))))

(defn burn [snuffer coll]
  "strips the sequence until the predi
cate is true then returns the rest as a lazy sequence"
  (loop [
         fire (first coll)
         fuel (rest coll)]
    (if (empty? fuel)
      []
      (if (snuffer fire)
        fuel
        (recur (first fuel) (rest fuel))))))

(defn seek [is-needle haystack]
  "find a needle in a haystack"
  (loop
      [hay (first haystack)
       stack (rest haystack)]
    (if (empty? stack)
      nil
      (if (is-needle hay)
        hay
        (recur (first stack) (rest stack))))))


(defn ring-around [the-rosy]
  "the rosy is a lazy sequence that at some point cycles, now we want to take a single cycle from the first element"
  (let
      [the-pocket (first the-rosy)
       ring       (rest the-rosy)]
      (loop
          [
           the-ring [the-pocket]
           around   ring]
        (if (= the-pocket (first around))
          the-ring
          (recur (conj the-ring (first around)) (rest around))))))


(defn bling
  ([init arc ordering]
     (let
         [the-king (nth arc init)
          arcle    (fn [item] (contains? (set arc) item))
          ]
       (loop ; loop until we catch the king
           [line-fore (burn #(= the-king %) ordering)
            culprit   (seek arcle line-fore)
            completion [the-king]
            ]
         (if (= culprit the-king)
           completion
           (let [new-fore (burn arcle line-fore)
                 new-culprit  (seek arcle new-fore)]
             (recur new-fore new-culprit (conj completion culprit))
             )
           )
         )
       ))
  ([arc ordering] (bling 0 arc ordering))
  )

(defn arcizer
  "get the arcizer function that will take integer inputs and return contextualized midi note values
  its arguments are all normalized inputs for use with a MIDI knobs"
  [position width basis octave]
  (let
      [apos (int (* 11 position))
       awidth (int (inc (* 10 width)))
       abasis (int (* (dec awidth) basis))
       aoctave (int (+ 2 (* 5 octave)))
       arc   (circle-arc apos awidth)
       notes (bling abasis arc (cycle circle-of-halves))
       degree (count notes)
       basis-note (p/NOTES(nth arc abasis))]
    (comment
      (println "notes:" notes)
      (println "arc: "  arc)
      (println "basis: " basis-note))
    (fn [x]
      (let
          [i (mod x degree)
           q (quot x degree)
           o (+ aoctave q)
           selected-note (p/NOTES (nth notes i))
           +from-basis (mod (- selected-note basis-note) 12)
           f (+ basis-note +from-basis)
           ]
        ;(println "x = " x "  i: " i "   q: " q "   o: " o "   seln: " selected-note "  +fb:" +from-basis  "   f: " f)
        (+ (* 12 o) f)))))


(defn arc-control
  "ctlmapping is a mapping from the control note value to the arcizer parameter to get

  eg{1 :pos 2 :wid 3 :base 4 :oct}

  this allows us to map any control onto these four params that are the 0 1 2 3 arguments of arcizer construction"
  ([ctlmapping]
      (let
          [possies {:pos 0 :wid 1 :base 2 :oct 3}
           a (atom (arcizer 0.5 0.5 0.5 0.5))
           args (atom [0.5 0.5 0.5 0.5])]

        {:ctl (fn [{note :note val :velocity-f}]
                (println "arcctl note: " note "  arcvel:  " val)
                (if (not (nil? (possies (ctlmapping note))))
                  (do
                    (swap! args #(assoc % (possies (ctlmapping note)) val))
                    (reset! a (apply arcizer @args))))
                )
         :arcizer a
         :sub {:on (fn [x] (@a x))}}
        ))
  ([] (arc-control {1 :pos 2 :wid 3 :base 4 :oct}))
  )

(comment
  (def atrol (arc-control {1 :pos 2 :wid 3 :base 4 :oct}))
  ((:on (:sub atrol)) 4)
  ((:ctl atrol) {:note 3 :velocity-f 1.0}))
