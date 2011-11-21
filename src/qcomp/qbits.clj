(ns qcomp.qbits
  (use incanter.core)
  )

(defn to-qbit [a]
         (cond 0 (matrix [1 0] 1)
               1 (matrix [0 1] 1)))

(defn assoc-m ([m x y v]
            (let [row (assoc (m y) x v)]
              (assoc m y row)))
         ([m x y v & params]
            (apply assoc-m (assoc-m m x y v) params)))

(defn f-to-matrix-1 [f]
               (assoc-m (matrix (repeat 4 0) 2)
                        0 (f 0) 1
                        1 (f 1) 1))

(defn m-to-int [x y]
  (+ (bit-shift-left x) y))

(defn f-to-matrix-2 [f]
  (assoc-m (matrix (repeat 16 0) 4)
           0 (m-to-int (f 0 0)) 1
           1 (m-to-int (f 0 1)) 1
           2 (m-to-int (f 1 0)) 1
           3 (m-to-int (f 1 1)) 1))

(defn f-to-matrix [f qbits]
  (if (:qfun (meta f))
    f
    (case qbits
      1 (f-to-matrix-1 f)
      2 (f-to-matrix-2 f)))

(def ^:qfun h (matrix [[(sqrt 1/2) (sqrt 1/2)]
                       [(sqrt 1/2) (* -1 (sqrt 1/2))]]))

(defn (qgate [f] (f-to-matrix f 2))
      (qgate [f1 f2]
             (kronecker (f-to-matrix f1 1)
                        (f-to-matrix f2 1))))
             