(defproject space-ed "1.0.0"
  :description "Fucking around with sounds and junk"
  :dependencies 
		[[org.clojure/clojure "1.5.1"]
		 [overtone "0.9.1"]
     [org.clojure/math.numeric-tower "0.0.4"]
     [quil "1.7.0"]
     [incanter "1.5.6"]
    ]
  :plugins  [[lein2-eclipse "2.0.0"]]
  :jvm-opts ^:replace []
  :main space-ed.core)
