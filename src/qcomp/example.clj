(ns qcomp.example
  (use qcomp.qbits)
  (use incanter.core))




(defn ufn [f] (fn [x y] [x (bit-xor y (f x))]))

(def eq identity)

(def swap #(- 1 %))
(def con0 (fn [_] 0))
(def con1 (fn [_] 1))

(defn deutsch [f] (compgate (qgate h h)
                            (qgate (ufn f))
                            (qgate h id)
                            ))


(defn run-deutsch [f] (read-qbit 0 (run 0 1 (deutsch f))))


  
