(ns space-ed.samples
  [:use overtone.core]
  [:require [overtone.samples.freesound :as fs]])

(defn get-random-selector [sample-array]
  (fn [x]
    (let [size (count sample-array)
          random (rand)]
      ((nth sample-array (int (* random size)))))))

(defn get-pressure-system [n]
  "returns a pressure system with n possible selections [0.. n-1]
   the system is a map with a get function that is called on itself,
   ((ps :get) ps)"
  (let
      [pressures (for [i (range n)] (atom 0))
       releases (for [i (range n)] (atom 0.9))
       buildups (for [i (range n)] (atom 0.1))]
    {:pressure pressures
     :release releases
     :buildup buildups
     :get     (fn [this]
                (let
                    [total-p  (apply + (for [a (this :pressure)] @a))
                     selector (* total-p (rand))
                     return   (loop
                                  [i   0
                                   sum @(first (this :pressure))]
                              (if (> sum selector)
                                i
                                (if (< (inc i) n)
                                  (recur (inc i) (+ sum @(nth (this :pressure) (inc i))))
                                  0))
                                )
                     ]

                  (doseq [j (range n)]
                    (if(= j  return)
                      (do (swap! (nth (this :pressure) j) #(* % @(nth (this :release) j))) (println "breaking: " j))
                      (swap! (nth (this :pressure) j) #(+ % (* (- 1 %) @(nth (this :buildup) j))))  ;(println "building: " j)
                      )
                    )
                  return

                  ))}
    )
  )

(def test-ps (get-pressure-system 5))


(defn load-my-samples []
  (do
    (defonce spacey-perc (freesound 150567))
    (defonce glow (freesound 169679))
    (defonce thunk (freesound 33156))
    (defonce blash (freesound 186965))
    (defonce clang (freesound 198115))
    (defonce shot (freesound 186952))
    (defonce bang (freesound 157216))
    (defonce ting (freesound 451))
    (defonce ding (freesound 160672))
    (defonce zoom (freesound 196106))
    (defonce slam (freesound 29542))
    (defonce gamek (freesound 168839))
    (defonce pisk (freesound 168842))
    (defonce boom (freesound 162850))
    (defonce stryk (freesound 168836))
    (defonce pisk2 (freesound 168835))
    (defonce rattle (freesound 178656))
    (defonce zap (freesound 169686))
    (defonce burst (freesound 186926))
    (def ride (sample(freesound-path 803)))
    (def kick (sample(freesound-path 777)))
    (def open-hat (sample (freesound-path 26657)))
    (def thump (sample (freesound-path 414)))
    (def rimmy-snare (sample (freesound-path 442)))
    (def semi-kick (sample (freesound-path 26894)))
    (def space-amb (sample (freesound-path 40165)))
    (def plop (sample (freesound-path 53763)))
    (def ultra-fat (sample (freesound-path 40616)))
    (def kick-pzhr (sample (freesound-path 57540)))
    (def jazzy-snare (sample (freesound-path 57534)))
    (def closed-hi-hat (sample (freesound-path 45664)))
    (def court-slide (sample (freesound-path 190812)))
    (def lazers (sample (freesound-path 94159)))
    (def balloon-squidge (sample (freesound-path 191011)))
    (def bloop-jelly (sample (freesound-path 191101)))

    (def onomatapaea [glow thunk blash clang shot bang ting ding zoom slam gamek pisk boom stryk pisk2 rattle zap burst])
    (def kit [kick ride open-hat closed-hi-hat jazzy-snare ])
    (def random-onomatapaea (get-random-selector onomatapaea))

    (def onomator (get-pressure-system (count onomatapaea)))

    (def onomatator (fn [x] ((onomatapaea ((onomator :get) onomator)))))

    (defn onomataplayer [i] ((onomatapaea i)))))

(defn get-sample-player [samples] 
  {:on (fn [e] 
         (if (and (contains? e :note) (not (coll? (e :note)))) 
           ((nth samples (e :note)))
           (if (contains? e :notes) 
             (doseq [n (e :notes)] ((samples n)))
             (doseq [n (e :note)] ((nth samples n))))))})

(load-my-samples)

(def kitted (get-sample-player kit))

((kitted :on) {:note 0}) 

