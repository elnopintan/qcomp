(ns qcomp.qbits
  (use incanter.core)
  (use qcomp.complex))

(defn to-qbit [a]
  (with-meta 
    (if (vector? a)
      (let [ [c1 c2] a]
        [(cnumber c1) (cnumber c2)])
      (case a
        0 [(cnumber 1) (cnumber 0)] 
        1 [(cnumber 0) (cnumber 1)] )) {:tag :Qbit}))



(defn replace-cell [m x y value]
  (assoc m y (assoc (m y) x value)))
      

(defn assoc-m
  ([m x y v]
     (replace-cell m x y v))
  ([m x y v & params]
     (apply assoc-m (assoc-m m x y v) params)))

(defn comp-m [[m0 m1]]
  [(vec (map cnumber m0))
   (vec (map cnumber m1))])
  
(defn f-to-matrix-1 [f]
  (assoc-m (comp-m [[0 0]
                    [0 0]])
           0 (f 0) (cnumber 1)
           1 (f 1) (cnumber 1)))

(defn f-to-matrix-2 [f]
  (let [ [m00 n00] (f 0 0)
         [m01 n01] (f 0 1)
         [m10 n10] (f 1 0)
         [m11 n11] (f 1 1)]
    [
     (comp-m (assoc-m [[0 0 0 0][0 0 0 0]] 0 m00 1 1 m01 1 2 m10 1 3 m11 1))
     (comp-m (assoc-m [[0 0 0 0][0 0 0 0]] 0 n00 1 1 n01 1 2 n10 1 3 n11 1))]))

(defn m-to-int [[x y]]
  (+ (bit-shift-left x 1) y))

(defn f-to-matrix [f qbits]
  (if (:qfun (meta f))
    f
    (case qbits
      1 (f-to-matrix-1 f)
      2 (f-to-matrix-2 f))))



(defn comp-kronecker [[q00 q01] [q10 q11]]
  [(cprod q00 q10) (cprod q00 q11) (cprod q01 q10) (cprod q01 q11)])
  
(defn apply-gate
  ([[q0 q1] [[c00 c01][c10 c11]]]
     (to-qbit [(csum (cprod c00 q0) (cprod  c01 q1)) (csum (cprod  c10 q0) (cprod c11 q1))]))
  ([q0 q1 [[c000 c001 c010 c011][c100 c101 c110 c111]]]
     (let [[q00 q01 q10 q11] (comp-kronecker q0 q1)]
       (to-qbit [(csum (cprod c000 q00) (cprod c001 q01) (cprod c010 q10) (cprod c011 q11))
                 (csum (cprod c100 q00) (cprod c101 q01) (cprod c110 q10) (cprod c111 q11))]))))

(def h (fn [q] (apply-gate q (comp-m [[(/ 1 (sqrt 2)) (/ 1 (sqrt 2))]
                                      [(/ 1 (sqrt 2)) (* -1 (/ 1 (sqrt 2)))]]))))  
(defn qgate
  ([f n]
      (case n
         1 (fn [qbit] (apply-gate qbit (f-to-matrix f 1)))
         2 (fn [q1 q2] (let [[m1 m2] (f-to-matrix f 2)]
                          [(apply-gate q1 q2 m1)
                           (apply-gate q1 q2 m2)]))))
       
  ([f1 f2 n]
     (fn [qbit1 qbit2] [((qgate f1 1) qbit1) (qgate f2 (qgate f2 1))])))



(defn run [x y gate]
  (gate (kronecker
               (to-qbit x)
               (to-qbit y))))