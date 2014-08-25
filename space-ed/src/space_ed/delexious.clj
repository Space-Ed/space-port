(ns space-ed.delexious)

(def listo (atom (list mod 'i 3 (list + 2 'i)) ))

(eval @listo)

(defn recurplace [replmap coll]
  "recursively replace the values inside the given collection so calls itself on collection elements that are collections"
  (for [x coll]
    (if (coll? x)
      (recurplace replmap x)
      (if (not (nil? (replmap x)))
        (replmap x)
        x)))
  )

(recurplace {'i 'x} @listo)

(replace {'i 'x nil 'x} @listo)

(def that @listo)

(recurplace )

(defmacro macreplace [listom i x]
  (replace {i x} listom))

(defmacro rec-macreplace [listom i x]
  (for [expr listom]
    (cond
     (= expr nil) x
     (= expr ~i)   x
     (seq? expr) (rec-macreplace expr ~i x))))

(defmacro bi-replace [listom i x]
  "replacing the symbol i with the value x"
  (first listom))

(swap! listo )

(def start (atom (list nil)))

(swap! start #(replace {nil 'i} %))  ;the call to replace i with

;;now we need a replace all and evaluate

(defn extract [i listo]
  (eval (replace {'i (fn [] i)} listo)))

(extract 3 @start)
