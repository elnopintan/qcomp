(ns qcomp.example
  (use qcomp.qbits))


(defn ufn [f] (fn [x y] [x (bit-xor y (f x))]))

(def eq identity)
(def swap #(- 1 %))
(def con0 (fn [_] 0))
(def con1 (fn [_] 1))


(defn deutsch [f]
  (let [ ufnres ((qgate (ufn f) 2) (h (to-qbit 0)) (h (to-qbit 1)))]
    [(h (first ufnres)) (second ufnres)]))


  
