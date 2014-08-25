(ns space-ed.looping
  "This namespace is used for defining the abstract sequencing functions, they should be primed to work with a variety of arguments including dynamic elements "
  [use overtone.core]
  [require [space-ed.dividex :as dvx]])

(defn test-selection [x]
  0)

(defn build-dynamic-pressure-system [uptake-proportion release-proportion]
  "A system which builds pressure, increasing the probability of a break until it breaks and pressure is released
   (return-value :break?) gives the non deterministic break predicate
   the predicate returns 0 for a break and 1 for a hold"
  (let
      [uptake (atom uptake-proportion)
       release (atom release-proportion)
       level  (atom 0)
       ]
    {:uptake uptake
     :release release
     :level   level
     :break?  (fn [] (if (> @level (rand))
                      (do
                        (swap! level #(* % @release))
                        true
                        )
                      (do
                        (swap! level #(+ % (* (- 1 %) @uptake)))
                        false
                        )
                      )
                )
     })
  )

(def example-dps (build-dynamic-pressure-system 0.1 0.5))

(defn get-dps-control [dps indexed-control]
  "interfaces to the singular dps, it will then activate a percussive noise only when pressure is released "
  (fn [x]
    (if ((dps :break?))
        (indexed-control x)))
 )

(defn start-pattern
  "Begin a looping in the given pattern (a sequence of quotients on the unit interval)
   will fire a selection function that must take the index of the beat this
   selection function will output the argument for the control functions

   selection output should be integers within the control set or  "
  [period-ms pattern selection-f control-set]
  (let [moment  (now)              ;;the progress in number of beats where this
        pattern (vec pattern)
        ticks   (range (count pattern))]
    ;;the play functions fit in here
    (doseq [i ticks ] (at (+ moment (* period-ms (pattern i))) (control-set (selection-f i))) )
    (apply-at (+ moment period-ms)  start-pattern period-ms pattern selection-f control-set [])
    )
  )

(def dynamic-pattern-memory (atom 1000))

(defn start-dynamic-pattern
  "this is a looper which implements time shift capabilities based off the differential of the periods.
   the period is therefore an atom and the function has priviledge to "
  ([time-control pattern selection-f control-set]
      (let [moment  (now)              ;;the progress in number of beats where this
            pattern (vec pattern)
            ticks   (range (count pattern))
            period1 @dynamic-pattern-memory
            period2 @time-control
            ]

        ;;the play functions fit in here

        (doseq [i ticks ] (at (+ moment (+ (* period1 (pattern i))
                                           (* (pattern i) (pattern i)(- period2 period1) )))
                              (control-set (selection-f i))) )                                ;;;the god line

        (reset! dynamic-pattern-memory period2)

        (apply-at (+ moment period2)  start-dynamic-pattern time-control pattern selection-f control-set [])
        ))

  )

(defn start-triggering-loop
  "similar to the above except triggers events instead of "
  ([time-control pattern id-key]
      (let [moment  (now)              ;;the progress in number of beats where this
            pattern (vec pattern)
            ticks   (range (count pattern))
            period1 @dynamic-pattern-memory
            period2 @time-control
            ]

        ;;the play functions fit in here

        (doseq [i ticks ] (at (+ moment (+ (* period1 (pattern i))
                                           (* (pattern i) (pattern i)(- period2 period1) )))
                              (event ::loop-trigger {:value i :id id-key})) )                                ;;;the god line

        (reset! dynamic-pattern-memory period2)

        (apply-at (+ moment period2)  start-triggering-loop time-control pattern [])
        )))

(comment
  (def time-stretch (atom 1000))
  (reset! time-stretch 1000)
  (start-triggering-loop time-stretch (dvx/dividex-32 16) :trig1)
  (on-event ::loop-trigger
            (fn [e] (println e))
            ::loop-trigger-listener))
