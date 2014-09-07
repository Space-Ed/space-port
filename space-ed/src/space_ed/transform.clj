(ns space-ed.transform
  
  "A namespace of event transformation definitions. for holding all the functions "
  )

(defn get-cross-map 
  "bridge the differences eg :note -> :sel with a function transforming the value"
  ([e-type key-from key-to f]
  {e-type (fn [e] (assoc e key-to (f (e key-from))))})
  ([e-type key-from key-to] (get-cross-map e-type key-from key-to (fn [e] e)))
 )
