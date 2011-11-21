(ns qcomp.qbits
  (use incanter.core)
  )

(defn to-qbit [a]
  (case a
    0 (matrix [1 0] 1)
    1 (matrix [0 1] 1)))

(defn replace-cell [m x y value]
  (matrix (map
           (fn [r n]
             (let [row (vec (map identity r))]
               (if (= y n) (assoc row x value) row))) m (range))))

(defn assoc-m
  ([m x y v]
     (replace-cell m x y v))
  ([m x y v & params]
     (apply assoc-m (assoc-m m x y v) params)))

(defn f-to-matrix-1 [f]
               (assoc-m (matrix (repeat 4 0) 2)
                        0 (f 0) 1
                        1 (f 1) 1))

(defn m-to-int [[x y]]
  (+ (bit-shift-left x 1) y))

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
      2 (f-to-matrix-2 f))))

(def h (with-meta (matrix [[(/ 1 (sqrt 2)) (/ 1 (sqrt 2))]
                       [(/ 1 (sqrt 2)) (* -1 (/ 1 (sqrt 2)))]]) {:qfun true}))

(defn qgate
  ([f] (f-to-matrix f 2))
  ([f1 f2]
     (kronecker (f-to-matrix f1 1)
                (f-to-matrix f2 1))))

(def id (with-meta (identity-matrix 2) {:qfun true}))

(defn compgate [& gates]
  (fn [qbits]
    (reduce (fn [q gate] (mmult gate q)) qbits gates)))

(defn run [x y gate]
  (gate (kronecker
               (to-qbit x)
               (to-qbit y))))