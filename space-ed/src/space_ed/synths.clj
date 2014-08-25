(ns space-ed.synths
  (:use [overtone.core])
  (:require [overtone.speech :as speech]))

(boot-external-server)

(defn get-gate-controller-m2 [inst]
  (fn [x]
    (ctl inst :gate (mod x 2))))

(defn get-frequency-controller [inst basis quantization]
  (fn [x]
    (ctl inst :freq (quantization (+ x basis)))))


(defsynth fm-gogo 
  [base-f 220
   mod1-dr 1
   mod1-fr 1   
   ]
  (let 
    [mod1-osc (sin-osc (/ base-f mod1-fr))
     mod1-mod (* mod1-osc (/ base-f mod1-dr))
     base-osc (sin-osc (+ base-f mod1-mod))]
    (out fm-bus base-osc)))

(def fm1 (fm-gogo))
(kill fm1)

(ctl fm1 :mod1-dr 1 :mod1-fr 4)

(def fm-bus (audio-bus 1 ))

(defsynth fm3 
  [base-f 440
   fr12   1
   fr23   1
   
   dr21   1
   dr31   1
   dr32   1
    
   o1     1
   o2     0
   o3     0]
  (let [
        
        f3 (* base-f fr12 fr23) 
        S3 (sin-osc:ar f3)
        f2 (+ (* base-f fr12) (* S3 (- 1 (/ 1 dr32))))
        S2 (sin-osc:ar f2)
        f1 (+ base-f (* S3 (- 1 (/ 1 dr31))) (* S2 (- 1 (/ 1 dr21))))
        S1 (sin-osc:ar f1)
        o  (+ (* o1 S1)(* o2 S2)(* o3 S3))]
    (out fm-bus o)))

(defsynth my-synth
  [freq 220
   vel  0.6
   gate 1]
  (let
      [signal (in:ar fm-bus)
       env    (env-gen:ar (adsr 0.6 0.4 1 0.2) gate FREE)
       filtenv  (env-gen:kr (adsr 1 0.4 1 0.2) gate FREE)
       filtered (lpf signal (+ 800 (* 2000 filtenv)))
       enved  (* signal filtered env)
       ]
    (out [0 1] (* vel enved))
    )
  )

(def my-synthstance (my-synth))
(kill my-synthstance)
(ctl my-synthstance :gate 0)
(ctl my-synthstance :gate 1)

(node-active? my-synthstance)
(node-live? my-synthstance)
(node-tree-seq my-synthstance)

(defsynth the-generica
  [freq 440
   vel  1
   gate 1
   filter-cut 500
   filter-lfo-f 30
   filter-lfo-d 100
   delay-time 1
   delay-decay 0.5]
  (let
      [
       delay-in (local-in)
       src    (pulse:ar freq)
       filt   (lpf src (+ filter-cut (* filter-lfo-d (sin-osc:kr filter-lfo-f))))
       delayed (delay-n (+ filt
                           (* delay-decay
                              delay-in))
                        delay-time
                        delay-time)

       -       (local-out delayed)
       env    (env-gen:kr (adsr 0.5 1.0 0.5 0.5) gate FREE)
       final  (* env vel (+ filt delayed))]
    (out [0 1] final)
    ))

(definst wsaw [note 60 vel 1 gate 1]
  (let [freq (midicps note)
        amp vel
        wob-env (env-gen (envelope [0.25 20] [13] :exp))
        fenv (env-gen (envelope [freq (* 16 freq)] [13] :exp))
        wob (+ fenv (* (* fenv 0.75)(sin-osc wob-env)))
        snd (saw wob)
        env (env-gen (adsr 0 0 1 0) gate :action FREE)]
    (* wob amp env snd)))

;(def a-synth (atom (the-generica :gate 1 :filter-lfo-f 10)))


(defn harmonics [fundamental num-harms]
  (let [overtone-nums (range num-harms)]
    (map #(* fundamental %) (rest overtone-nums))))

(defmacro harm-series [fundamental num-harms]
   `(vec (harmonics ~fundamental ~num-harms)))

(comment
  (demo 5
    (out [0 1] (let
        [
            s (sin-osc:kr  2)
            tr1 (> s 0)
            tr2 (< s 0)
            env1 (env-gen (perc 0 0.125) :gate tr1)
            env2 (env-gen (perc 0 0.125) :gate tr2)
            sound1 (* env1 (sin-osc 440))
            sound2 (* env2 (sin-osc 550))
            step1 (stepper:kr tr1)
            _ (poll:kr tr1 step1 "stepper 1: ")
            step2 (stepper:kr tr2)
            _ (poll:kr tr2 step2 "stepper 2: ")
        ]
    [sound1 sound2])))
 )

(definst harmonia
  [freq 440
   vel 0.5
   gate 1.0]
  (let [env        (env-gen (adsr 1.0 0.5 0.0 0.5) gate 1 0 1 FREE)
        outie      (sin-osc (* freq [1 3/2]))]
    (* env outie)))




(definst pad2 [freq 440 vel 0.4 amt 0.3 gate 1.0]
  (let [vel        (+ 0.5 (* 0.5 vel)) ;;minimum vel 0.5
        env        (env-gen (adsr 0.0 0.1 0.7 0.5) gate 1 0 1 FREE)  ; envelope for amplitude
        f-env      (env-gen (perc 1 3))  ;filter envelope
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.2 freq) (* f-env 2 freq)) 0.2)  ;cutoff
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (* vel env echo)))


;; Tuned-Detuned Synth
;;
;; Mouse X direction controls the base frequency
;; Mouse Y direction controls the LFO which controls the saw tooth
;;       output that modifies the base frequency slightly.
;;
;; Mouse Y also controls a LPF on the mixed audio signal.
;;
;;
(defsynth tdt-synth [freq 440
                     amp 0.8
                     pan-rate 0.5]
  (let [lfo-rate (mouse-y 0 30)
        lfo-pan (sin-osc:kr pan-rate)
        freq-a (/ freq 2)               ; Octave below
        freq-b (/ (* (/ 3 2) freq) 2)   ; Octave Below, 5th above
        freq-c (* (/ 4 3) freq)         ; Perfect 4th
        basef-mod (lin-lin:kr (lf-saw:kr lfo-rate) -1.0 1.0 0 10)
        mud (lin-lin:kr (lf-noise0:kr 15) -1 1 -5 5)
                                        ; Adds a some wobble.
        basef (mouse-x 50 1000)
        signal (+ (* 0.8 (sin-osc (+ basef freq-a basef-mod)))
                  (* 0.5 (square  (+ mud basef freq-b basef-mod)))
                  (* 0.7 (saw     (+ mud basef freq-c basef-mod))))]
    (out 0 :signals (pan2
                     (* amp (lpf:ar signal (mouse-y 500 4000 EXP)))
                     :pos (* 0.75 lfo-pan) ; Reduce Panning
                     ))))

;; Move mouse to the upper left to hear the "base" sound.
;; Reminiscent of a Boards of Canada type drone synth
(comment
  (tdt-synth :freq 150
             :amp 0.8
             :pan-rate 0.24))


;; translated from: https://github.com/supercollider-quarks/SynthDefPool/blob/master/pool/apad_mh.scd
(definst simple-flute [freq 880
                       amp 0.5
                       attack 0.4
                       decay 0.5
                       sustain 0.8
                       release 1
                       gate 1
                       out 0]
  (let [env  (env-gen (adsr attack decay sustain release) gate :action FREE)
        mod1 (lin-lin:kr (sin-osc:kr 6) -1 1 (* freq 0.99) (* freq 1.01))
        mod2 (lin-lin:kr (lf-noise2:kr 1) -1 1 0.2 1)
        mod3 (lin-lin:kr (sin-osc:kr (ranged-rand 4 6)) -1 1 0.5 1)
        sig (distort (* env (sin-osc [freq mod1])))
        sig (* amp sig mod2 mod3)]
    sig))

;;modified version of: https://github.com/supercollider-quarks/SynthDefPool/blob/master/pool/cs80lead_mh.scd
(definst cs80lead
  [freq 880
   amp 0.5
   att 0.75
   decay 0.5
   sus 0.8
   rel 1.0
   fatt 0.75
   fdecay 0.5
   fsus 0.8
   frel 1.0
   cutoff 200

   dtune 0.002
   vibrate 4
   vibdepth 0.015
   gate 1
   ratio 1
   cbus 1
   freq-lag 0.1]
  (let [freq (lag freq freq-lag)
        cuttoff (in:kr cbus)
        env     (env-gen (adsr att decay sus rel) gate :action FREE)
        fenv    (env-gen (adsr fatt fdecay fsus frel 2) gate)

        vib     (+ 1 (lin-lin:kr (sin-osc:kr vibrate) -1 1 (- vibdepth) vibdepth))

        freq    (* freq vib)
        sig     (mix (* env amp (saw [freq (* freq (+ dtune 1))])))]
    sig))


(definst overpad
  [freq 440 amp 0.7 attack 0.001 release 2]
  (let [
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    audio))
 


(definst buzz
  [pitch 40 cutoff 300 dur 200]
  (let [lpf-lev (* (+ 1 (lf-noise1:kr 10)) 400)
        a       (lpf (saw (midicps pitch)) lpf-lev)
        b       (sin-osc (midicps (- pitch 12)))
        env     (env-gen 1 1 0 1 2 (perc 0.01 (/ dur 1000)))]
    (* env (+ a b))))

(definst bass
  [freq 120 t 0.6 amp 0.5 gate 1]
  (let [env  (env-gen (perc 0.08 t) gate :action FREE)
        src  (saw [freq (* 0.98 freq) (* 2.015 freq)])
        src  (clip2 (* 1.3 src) 0.8)
        sub  (sin-osc (/ freq 2))
        filt (resonz (rlpf src (* 4.4 freq) 0.09) (* 2.0 freq) 2.9)]
    (* env amp (fold:ar (distort (* 1.3 (+ filt sub))) 0.08))))

(definst daf-bass [freq 440 gate 1 amp 0.5 out-bus 0 ]
  (let [harm [1 1.01 2 2.02 3.5 4.01 5.501]
        harm (concat harm (map #(* 2 %) harm))
        snd  (* 2 (distort (sum (sin-osc (* freq harm)))))
        snd  (+ snd (repeat 2 (sum (sin-osc (/ freq [1 2])))))
        env  (env-gen (adsr 0.001 0.2 0.9 0.25) gate amp :action FREE)]
    (* snd env)))

(definst grunge-bass
  [freq 440 amp 1 dur 0.1 a 0.01 d 0.01 s 0.4 r 0.01]
  (let [
        env     (env-gen (adsr a d s r) (line:kr 1 0 (+ a d dur r 0.1))
                         :action FREE)
        src     (saw [freq (* 0.98 freq) (* 2.015 freq)])
        src     (clip2 (* 1.3 src) 0.9)
        sub     (sin-osc (/ freq 2))
        filt    (resonz (rlpf src (* 8.4 freq) 0.29) (* 2.0 freq) 2.9)
        meat    (ring4 filt sub)
        sliced  (rlpf meat (* 2 freq) 0.1)
        bounced (free-verb sliced 0.8 0.9 0.2)]
    (* env bounced)))

(definst vintage-bass
  [freq 440 velocity 80 t 0.6 amp 0.5 gate 1]
  (let [
        sub-freq (* 0.5 freq)
        velocity (/ velocity 127.0)
        sawz1    (* 0.075 (saw [freq freq]))
        sawz2    (* 0.75 (saw [(- freq 2) (+ 1 freq)]))
        sqz      (* 0.3 (pulse [sub-freq (- sub-freq 1)]))
        mixed    (* 0.1 (mix sawz1 sawz2 sqz))
        env      (env-gen (adsr 0.1 3.3 0.4 0.8) gate :action FREE)
        filt     (* env (moog-ff mixed (* velocity env (+ freq 200)) 2.2))]
    filt))

; B3 modeled a church organ using additive synthesis of 9 sin oscillators
; * Octave under root
; *	Fifth over root
; * Root
; * Octave over root
; * Octave and a fifth over root
; * Two octaves over root
; * Two octaves and a major third over root
; * Two octaves and a fifth over root
; * Three octaves over root
; Work in progress...  just getting started
(definst b3
  [freq 440 a 0.01 d 3 s 1 r 0.01 gate 1]
  (let [
        waves (sin-osc [(* 0.5 freq)
                        freq
                        (* (/ 3 2) freq)
                        (* 2 freq)
                        (* freq 2 (/ 3 2))
                        (* freq 2 2)
                        (* freq 2 2 (/ 5 4))
                        (* freq 2 2 (/ 3 2))
                        (* freq 2 2 2)])
        snd   (apply + waves)
        env   (env-gen (adsr a d s r) gate :action FREE)]
    (* env snd 0.1)))


(definst ks-stringer
  [freq 440 rate 6]
  (let [noize (* 0.8 (white-noise))
        trig  (dust rate)
        coef  (mouse-x -0.999 0.999)
        delay (/ 1.0 (* (mouse-y 0.001 0.999) freq))
        plk   (pluck noize trig (/ 1.0 freq) delay 10 coef)
        filt (rlpf plk (* 12 freq) 0.6)]
    (* 0.8 filt)))

(definst fm-demo
  [note 60 amp 0.2 gate 0]
  (let [freq (midicps note)
        osc-a (* (sin-osc (mouse-x 20 3000))
                 0.3)
        osc-b (* amp (sin-osc (* (mouse-y 3000 0) osc-a)))]
    osc-a))

; From the SC2 examples included with SC
; Don't think it's quite there, but almost...
(definst harmonic-swimming
  [amp 0.5]
  (let [freq     100
        partials 20
        z-init   0
        offset   (line:kr 0 -0.02 60)
        snd (loop [z z-init
                   i 0]
              (if (= partials i)
                z
                (let [f (clip:kr (mul-add
                                   (lf-noise1:kr [(+ 6 (rand 4))
                                                  (+ 6 (rand 4))])
                                   0.2 offset))
                      src  (f-sin-osc (* freq (inc i)))
                      newz (mul-add src f z)]
                  (recur newz (inc i)))))]
    (out 10 (pan2 (* amp snd)))))

(definst whoahaha
  [freq 440 dur 5 osc 100 mul 1000]
  (let [freqs [freq (* freq 1.0068) (* freq 1.0159)]
        sound (resonz (saw (map #(+ % (* (sin-osc osc) mul)) freqs))
                      (x-line 10000 10 25)
                      (line 1 0.05 25))
        sound (apply + sound)]
  (* (lf-saw:kr (line:kr 13 17 3)) (line:kr 1 0 dur FREE) sound)))

(definst bubbles
  [bass-freq 80]
  (let [bub (+ bass-freq (* 3 (lf-saw:kr [8 7.23])))
        glis (+ bub (* 24 (lf-saw:kr 0.4 0)))
        freq (midicps glis)
        src (* 0.04 (sin-osc freq))
        zout (comb-n src :decay-time 4)]
    zout))

; // Originally from the STK instrument models...
(definst bowed
  [freq 440 velocity 80 gate 1 amp 1
   bow-offset 0 bow-slope 0.5 bow-position 0.75 vib-freq 6.127 vib-gain 0.2]
  (let [
        velocity     (/ velocity 127)
        beta-ratio   (+ 0.027236 (* 0.2 bow-position))
        base-delay   (reciprocal freq)
        [fb1 fb2]    (local-in 2)
        vibrato      (* (sin-osc vib-freq) vib-gain)
        neck-delay   (+ (* base-delay (- 1 beta-ratio)) (* base-delay vibrato))
        neck         (delay-l fb1 0.05 neck-delay)
        nut-refl     (neg neck)
        bridge       (delay-l fb2 0.025 (* base-delay beta-ratio))
        string-filt  (one-pole (* bridge 0.95) 0.55)
        bridge-refl  (neg string-filt)
        adsr         (* amp (env-gen (adsr 0.02 3.005 1.0 0.01) gate :action FREE))
        string-vel   (+ bridge-refl nut-refl)
        vel-diff     (- adsr string-vel)
        slope        (- 5.0 (* 4 bow-slope))
        bow-table    (clip:ar (pow (abs (+ (* (+ vel-diff bow-offset) slope) 0.75 )) -4) 0 1)
        new-vel       (* vel-diff bow-table)]
   (local-out (+ [bridge-refl nut-refl] new-vel))
   (resonz (* bridge 0.5) 500 0.85)))

(comment definst flute
  [gate 1
   freq 440
   amp 1.0
   endreflection 0.5
   jetreflection 0.5
   jetratio 0.32
   noise-gain 0.15
   vibfreq 5.925
   vib-gain 0.0
   amp 1.0]
  (let [nenv           (env-gen (linen 0.2 0.03 0.5 0.5) :action FREE)
        adsr           (+ (* amp 0.2) (env-gen (adsr 0.005 0.01 1.1 0.01) gate :action FREE))
        noise          (* (white-noise) noise-gain)
        vibrato        (sin-osc vibfreq 0 vib-gain)
        delay          (reciprocal (* freq 0.66666))
        lastout        (local-in 1)
        breathpressure (* adsr (+ noise, vibrato))
        filter         (leak-dc (one-pole (neg lastout) 0.7))
        pressurediff   (- breathpressure (* jetreflection filter))
        jetdelay       (delay-l pressurediff 0.025 (* delay jetratio))
        jet            (clip2 (* jetdelay (- (squared jetdelay) 1.0)) 1.0)
        boredelay      (delay-l (+ jet (* endreflection filter) 0.05 delay))]
    (local-out boredelay)
    (* 0.3 boredelay amp nenv)))
